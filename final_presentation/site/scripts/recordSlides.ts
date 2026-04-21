/**
 * Record each deck slide to its own video file using Playwright.
 *
 * Workflow per slide:
 *   1. Open a fresh browser context with video recording at 1920×1080.
 *   2. Navigate to ?slide=N&step=1.
 *   3. Wait for fonts.ready and any Player-bearing element to mount.
 *   4. Hide deck chrome (press 'h') and dwell.
 *   5. For each subsequent build step, press ArrowRight and dwell.
 *   6. Close the context — Playwright finalises the .webm.
 *   7. Move the auto-named webm to videos/<id>.webm.
 *
 * The recorder reads window.__DECK__ (populated by Deck.tsx) for the
 * authoritative slide manifest, and each Player slide tags its container
 * with `data-record-dwell-ms` so we wait long enough for one full loop.
 */

import { spawn, type ChildProcess } from 'node:child_process';
import { mkdir, readdir, rename } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { chromium, type BrowserContext } from 'playwright';

const __dirname = dirname(fileURLToPath(import.meta.url));
const SITE_DIR = resolve(__dirname, '..');
const OUT_DIR = resolve(SITE_DIR, 'out/videos-webm');

// Bun + Playwright disagree on page.waitForTimeout (it throws
// "Target page closed" mid-wait), so we use a plain timer.
const sleep = (ms: number): Promise<void> =>
  new Promise((res) => setTimeout(res, ms));
// Chromium's screen recorder fails silently when the recordVideo dir
// contains spaces or unicode characters in some macOS setups (notably
// iCloud-synced paths). Stage videos in a clean tmp dir then move them
// into the final out dir.
const STAGING_DIR = resolve(tmpdir(), 'incollege-deck-videos');

const DEFAULT_INITIAL_DWELL_MS = 1500;
const DEFAULT_STEP_DWELL_MS = 1800;
const DEFAULT_TAIL_DWELL_MS = 600;

type SlideManifestEntry = {
  id: string;
  title: string;
  steps: number;
  breakout: boolean;
};

type CliArgs = {
  only?: Set<number>;      // 1-based slide indices to record
  baseUrl?: string;        // pre-existing dev server (skip spawn)
  headed: boolean;
  initialDwellMs: number;
  stepDwellMs: number;
};

function parseArgs(argv: readonly string[]): CliArgs {
  const args: CliArgs = {
    headed: false,
    initialDwellMs: DEFAULT_INITIAL_DWELL_MS,
    stepDwellMs: DEFAULT_STEP_DWELL_MS,
  };
  for (const arg of argv) {
    if (arg === '--headed') args.headed = true;
    else if (arg.startsWith('--only=')) {
      args.only = new Set(
        arg
          .slice('--only='.length)
          .split(',')
          .map((s) => Number.parseInt(s.trim(), 10))
          .filter((n) => Number.isFinite(n) && n > 0),
      );
    } else if (arg.startsWith('--baseUrl=')) {
      args.baseUrl = arg.slice('--baseUrl='.length);
    } else if (arg.startsWith('--initialDwellMs=')) {
      args.initialDwellMs = Number.parseInt(arg.slice('--initialDwellMs='.length), 10);
    } else if (arg.startsWith('--stepDwellMs=')) {
      args.stepDwellMs = Number.parseInt(arg.slice('--stepDwellMs='.length), 10);
    }
  }
  return args;
}

async function startDevServer(): Promise<{ url: string; child: ChildProcess }> {
  const port = await findFreePort(5174);
  const child = spawn(
    'bunx',
    ['vite', '--port', String(port), '--strictPort'],
    {
      cwd: SITE_DIR,
      env: { ...process.env, DECK_BASE: '/' },
      stdio: ['ignore', 'pipe', 'pipe'],
    },
  );

  let resolved = false;
  const url = `http://localhost:${port}/`;
  await new Promise<void>((resolveReady, reject) => {
    const onData = (data: Buffer) => {
      const text = data.toString();
      process.stdout.write(`[vite] ${text}`);
      if (!resolved && text.includes('ready in')) {
        resolved = true;
        resolveReady();
      }
    };
    child.stdout?.on('data', onData);
    child.stderr?.on('data', onData);
    child.once('exit', (code) => {
      if (!resolved) reject(new Error(`vite exited early (code=${code})`));
    });
    setTimeout(() => {
      if (!resolved) {
        resolved = true;
        // Some vite versions print "ready" with an emoji that may not match;
        // fall back to a generous timeout and assume the server is up.
        resolveReady();
      }
    }, 8000);
  });

  return { url, child };
}

async function findFreePort(start: number): Promise<number> {
  const net = await import('node:net');
  return new Promise((res, rej) => {
    const tryPort = (p: number) => {
      const server = net.createServer();
      server.unref();
      server.once('error', () => tryPort(p + 1));
      server.listen(p, () => {
        const addr = server.address();
        const port = typeof addr === 'object' && addr ? addr.port : p;
        server.close(() => res(port));
      });
      if (p > start + 100) rej(new Error('no free port'));
    };
    tryPort(start);
  });
}

async function fetchManifest(baseUrl: string): Promise<SlideManifestEntry[]> {
  const browser = await chromium.launch();
  const page = await browser.newPage();
  await page.goto(`${baseUrl}?slide=1`, { waitUntil: 'domcontentloaded' });
  await page.waitForFunction(
    () => Boolean((window as unknown as { __DECK__?: unknown }).__DECK__),
    null,
    { timeout: 30_000 },
  );
  const slides = await page.evaluate(() => {
    const deck = (window as unknown as { __DECK__: { slides: SlideManifestEntry[] } }).__DECK__;
    return deck.slides;
  });
  await browser.close();
  return slides;
}

async function recordSlide(
  baseUrl: string,
  slide: SlideManifestEntry,
  oneBasedIndex: number,
  args: CliArgs,
  outDir: string,
): Promise<void> {
  const slug = `${String(oneBasedIndex).padStart(2, '0')}-${slide.id}`;
  const targetWebm = resolve(outDir, `${slug}.webm`);
  if (existsSync(targetWebm)) {
    console.log(`[skip] ${slug} (webm already exists)`);
    return;
  }

  // Fresh browser per slide. Chromium's screencast pipeline can corrupt
  // the parent process when a context with recordVideo is closed, so a
  // fresh process per slide is the only stable pattern under Bun.
  const browser = await chromium.launch({ headless: !args.headed });
  let context: BrowserContext | null = null;
  try {
    context = await browser.newContext({
      viewport: { width: 1920, height: 1080 },
      deviceScaleFactor: 1,
      recordVideo: { dir: STAGING_DIR, size: { width: 1920, height: 1080 } },
    });
    const page = await context.newPage();

    const url = `${baseUrl}?slide=${oneBasedIndex}&step=1`;
    page.on('pageerror', (err) => console.error(`  [${slug}] page error: ${err.message}`));
    page.on('console', (msg) => {
      if (msg.type() === 'error') console.error(`  [${slug}] console: ${msg.text()}`);
    });
    await page.goto(url, { waitUntil: 'load' });

    await page.waitForFunction(
      () => Boolean((window as unknown as { __DECK__?: unknown }).__DECK__),
      null,
      { timeout: 15_000 },
    );

    await page.keyboard.press('h');
    await page.evaluate(async () => {
      if ('fonts' in document) await (document as Document & { fonts: { ready: Promise<unknown> } }).fonts.ready;
    });
    await sleep(200);

    // Pick the slide's required dwell from any [data-record-dwell-ms] hints.
    const hintedDwell = await page.evaluate(() => {
      const nodes = document.querySelectorAll<HTMLElement>('[data-record-dwell-ms]');
      let max = 0;
      nodes.forEach((n) => {
        const v = Number.parseInt(n.dataset.recordDwellMs ?? '0', 10);
        if (Number.isFinite(v) && v > max) max = v;
      });
      return max;
    });

    const initialDwell = Math.max(args.initialDwellMs, hintedDwell);
    await sleep(initialDwell);

    // Walk through additional build steps.
    for (let step = 2; step <= slide.steps; step += 1) {
      await page.keyboard.press('ArrowRight');
      // Step transitions are CSS 500ms; give the new content room to play
      // its own animations or a Player loop if it just appeared.
      await sleep(args.stepDwellMs);
      const stepHint = await page.evaluate(() => {
        const nodes = document.querySelectorAll<HTMLElement>('[data-record-dwell-ms]');
        let max = 0;
        nodes.forEach((n) => {
          const v = Number.parseInt(n.dataset.recordDwellMs ?? '0', 10);
          if (Number.isFinite(v) && v > max) max = v;
        });
        return max;
      });
      if (stepHint > args.stepDwellMs) {
        await sleep(stepHint - args.stepDwellMs);
      }
    }

    // A small tail so the final frame doesn't get clipped on the cut.
    await sleep(DEFAULT_TAIL_DWELL_MS);

    const video = page.video();
    await page.close();
    await context.close();
    context = null;

    if (video) {
      const auto = await video.path();
      await rename(auto, targetWebm);
      console.log(`[ok] ${slug} -> ${targetWebm}`);
    } else {
      console.warn(`[warn] no video recorded for ${slug}`);
    }
  } finally {
    if (context) await context.close().catch(() => {});
    await browser.close().catch(() => {});
  }
}

async function cleanupStrayWebms(outDir: string): Promise<void> {
  // Playwright sometimes leaves an empty webm if a context closes oddly.
  // Anything not following our naming pattern is fair game to discard.
  if (!existsSync(outDir)) return;
  const entries = await readdir(outDir);
  for (const e of entries) {
    if (!e.endsWith('.webm')) continue;
    if (/^\d{2}-/.test(e)) continue;
    await rename(resolve(outDir, e), resolve(outDir, `_orphan-${Date.now()}-${e}`)).catch(() => {});
  }
}

async function main(): Promise<void> {
  const args = parseArgs(process.argv.slice(2));
  await mkdir(OUT_DIR, { recursive: true });
  await mkdir(STAGING_DIR, { recursive: true });

  let serverChild: ChildProcess | undefined;
  let baseUrl = args.baseUrl;
  if (!baseUrl) {
    const server = await startDevServer();
    serverChild = server.child;
    baseUrl = server.url;
    // Give vite a moment to actually bind even after the "ready" line.
    await new Promise((r) => setTimeout(r, 600));
  }

  try {
    const manifest = await fetchManifest(baseUrl);
    console.log(`[manifest] ${manifest.length} slides`);

    for (let i = 0; i < manifest.length; i += 1) {
      const oneBased = i + 1;
      if (args.only && !args.only.has(oneBased)) continue;
      const slide = manifest[i];
      console.log(
        `[record] ${oneBased}/${manifest.length} · ${slide.id} · steps=${slide.steps}`,
      );
      await recordSlide(baseUrl, slide, oneBased, args, OUT_DIR);
    }

    await cleanupStrayWebms(OUT_DIR);
  } finally {
    if (serverChild) {
      serverChild.kill('SIGTERM');
      await new Promise((r) => setTimeout(r, 200));
    }
  }
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});

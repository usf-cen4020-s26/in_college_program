/**
 * Concatenate all per-slide mp4s into a single presentation video.
 * Uses ffmpeg's concat demuxer (stream-copy, no re-encode) so it's fast
 * and lossless — all inputs share the same codec/resolution from transcode.
 */

import { spawn } from 'node:child_process';
import { mkdir, readdir, writeFile, rm } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { tmpdir } from 'node:os';

const __dirname = dirname(fileURLToPath(import.meta.url));
const SITE_DIR = resolve(__dirname, '..');
const VIDEO_DIR = resolve(SITE_DIR, 'out/videos');
const OUT_VIDEO = resolve(SITE_DIR, 'out/incollege-presentation.mp4');

function ffmpeg(args: readonly string[]): Promise<void> {
  return new Promise((resolveOk, reject) => {
    const child = spawn('ffmpeg', args, { stdio: ['ignore', 'ignore', 'pipe'] });
    let stderr = '';
    child.stderr?.on('data', (d) => { stderr += d.toString(); });
    child.once('exit', (code) => {
      if (code === 0) resolveOk();
      else reject(new Error(`ffmpeg exited ${code}: ${stderr.slice(-600)}`));
    });
  });
}

async function main(): Promise<void> {
  if (!existsSync(VIDEO_DIR)) {
    console.error(`No videos at ${VIDEO_DIR}; run record + transcode first.`);
    process.exit(1);
  }

  const files = (await readdir(VIDEO_DIR))
    .filter((f) => f.endsWith('.mp4') && /^\d{2}-/.test(f))
    .sort();

  if (files.length === 0) {
    console.error(`No mp4 files found in ${VIDEO_DIR}.`);
    process.exit(1);
  }

  await mkdir(dirname(OUT_VIDEO), { recursive: true });

  // Write a concat list file. ffmpeg requires absolute paths when -safe 0 is set.
  const listPath = resolve(tmpdir(), `incollege-concat-${Date.now()}.txt`);
  const listContent = files.map((f) => `file '${resolve(VIDEO_DIR, f)}'`).join('\n');
  await writeFile(listPath, listContent, 'utf8');

  console.log(`[concat] ${files.length} slides -> ${OUT_VIDEO}`);

  await ffmpeg([
    '-y',
    '-f', 'concat',
    '-safe', '0',
    '-i', listPath,
    '-c', 'copy',
    OUT_VIDEO,
  ]);

  await rm(listPath, { force: true });
  console.log(`[done] wrote ${OUT_VIDEO}`);
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});

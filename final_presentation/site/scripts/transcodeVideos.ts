/**
 * Transcode each recorded slide webm into an mp4 that PowerPoint plays
 * natively (h264 + faststart). PowerPoint's media engine on macOS and
 * Windows is happiest with yuv420p baseline-ish h264 and a silent aac
 * track, so we add both.
 */

import { spawn } from 'node:child_process';
import { mkdir, readdir, stat } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const SITE_DIR = resolve(__dirname, '..');
const IN_DIR = resolve(SITE_DIR, 'out/videos-webm');
const OUT_DIR = resolve(SITE_DIR, 'out/videos');

function ffmpeg(args: readonly string[]): Promise<void> {
  return new Promise((resolveOk, reject) => {
    const child = spawn('ffmpeg', args, { stdio: ['ignore', 'ignore', 'pipe'] });
    let stderr = '';
    child.stderr?.on('data', (d) => {
      stderr += d.toString();
    });
    child.once('exit', (code) => {
      if (code === 0) resolveOk();
      else reject(new Error(`ffmpeg exited ${code}: ${stderr.slice(-400)}`));
    });
  });
}

async function transcode(input: string, output: string): Promise<void> {
  // -an because the deck has no audio; PowerPoint is happier with a silent
  // aac track than no track at all on some Windows builds, so we synth one.
  await ffmpeg([
    '-y',
    '-i', input,
    '-f', 'lavfi', '-i', 'anullsrc=channel_layout=stereo:sample_rate=44100',
    '-shortest',
    '-c:v', 'libx264',
    '-pix_fmt', 'yuv420p',
    '-profile:v', 'high',
    '-level', '4.2',
    '-preset', 'medium',
    '-crf', '20',
    '-movflags', '+faststart',
    '-c:a', 'aac',
    '-b:a', '96k',
    output,
  ]);
}

async function main(): Promise<void> {
  if (!existsSync(IN_DIR)) {
    console.error(`No webm directory at ${IN_DIR}; run record first.`);
    process.exit(1);
  }
  await mkdir(OUT_DIR, { recursive: true });

  const files = (await readdir(IN_DIR))
    .filter((f) => f.endsWith('.webm') && /^\d{2}-/.test(f))
    .sort();

  for (const file of files) {
    const input = resolve(IN_DIR, file);
    const output = resolve(OUT_DIR, file.replace(/\.webm$/, '.mp4'));
    if (existsSync(output)) {
      const [iStat, oStat] = await Promise.all([stat(input), stat(output)]);
      if (oStat.mtimeMs >= iStat.mtimeMs) {
        console.log(`[skip] ${file} (mp4 fresh)`);
        continue;
      }
    }
    process.stdout.write(`[ffmpeg] ${file} ... `);
    await transcode(input, output);
    process.stdout.write('done\n');
  }
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});

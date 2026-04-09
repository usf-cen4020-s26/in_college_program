import type { TerminalEvent } from '../remotion/compositions';

/**
 * Walk a Terminal script and compute its total runtime in milliseconds.
 *
 * Mirrors the accounting rules inside `Terminal.tsx::renderScript`:
 *   - `pause` adds its `ms` directly.
 *   - `print` adds its optional `delay` (defaults to 0).
 *   - `type` adds `chars.length * speed` (default speed 60 ms/char).
 *   - `clear` is instantaneous.
 *
 * Used by slides to size their embedded `<Player>` without each slide
 * hard-coding frame counts that would silently drift if the script
 * changes.
 */
export function scriptDurationMs(script: readonly TerminalEvent[]): number {
  let total = 0;
  for (const event of script) {
    switch (event.type) {
      case 'pause':
        total += event.ms;
        break;
      case 'print':
        total += event.delay ?? 0;
        break;
      case 'type':
        total += [...event.text].length * (event.speed ?? 60);
        break;
      case 'clear':
        break;
    }
  }
  return total;
}

/**
 * Convert the walked duration into Remotion frames, rounded up so the
 * final event always has a full frame to render. Adds a small tail so
 * the cursor blink keeps going for a beat after the last event.
 */
export function scriptDurationFrames(
  script: readonly TerminalEvent[],
  fps = 30,
  tailFrames = 30,
): number {
  const ms = scriptDurationMs(script);
  return Math.ceil((ms / 1000) * fps) + tailFrames;
}

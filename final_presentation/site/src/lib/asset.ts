/**
 * Resolves a public asset path relative to Vite's base URL so it works in
 * both `bun run dev` and on GitHub Pages (where the deck lives under a
 * `/repo-name/` subpath).
 *
 * Usage: `asset('assets/burndown_end.png')` or `asset('/assets/foo.png')`
 * — leading slashes are handled idempotently.
 */
export function asset(path: string): string {
  const base = import.meta.env.BASE_URL;
  const normalizedBase = base.endsWith('/') ? base : `${base}/`;
  const normalizedPath = path.startsWith('/') ? path.slice(1) : path;
  return `${normalizedBase}${normalizedPath}`;
}

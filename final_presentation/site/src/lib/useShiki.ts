import { useEffect, useState } from 'react';
import { getHighlighter } from '../shiki';

/**
 * React hook that highlights a code snippet with the shared Shiki
 * singleton. Returns `null` until the highlighter is ready, then the
 * highlighted HTML string. Re-runs whenever `code` changes; cleans up a
 * mounted flag on unmount so stale async results don't trigger warnings.
 *
 * Defaults to COBOL + the in-college-dark theme; override via args.
 */
export function useShikiHtml(
  code: string,
  lang: string = 'cobol',
  theme: string = 'in-college-dark',
): string | null {
  const [html, setHtml] = useState<string | null>(null);

  useEffect(() => {
    let mounted = true;
    setHtml(null);

    getHighlighter()
      .then((hl) => {
        if (!mounted) return;
        const out = hl.codeToHtml(code, { lang, theme });
        setHtml(out);
      })
      .catch(() => {
        // Swallow errors — the caller falls back to a plain <pre>.
      });

    return () => {
      mounted = false;
    };
  }, [code, lang, theme]);

  return html;
}

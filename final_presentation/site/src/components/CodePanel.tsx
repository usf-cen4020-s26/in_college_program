import { useShikiHtml } from '../lib/useShiki';

interface CodePanelProps {
  filePath: string;
  code: string;
  maxHeight?: number;
  lang?: string;
}

/**
 * Terminal-style wrapper around a Shiki-highlighted code snippet.
 * Shows three fake traffic lights on the left of a header bar with the
 * file path centered in mono text. The body uses the shared
 * `.code-panel` class so margins, borders, and font metrics match the
 * rest of the deck.
 *
 * The HTML injected via `dangerouslySetInnerHTML` comes from our
 * Shiki singleton, which produces trusted, escaped markup from
 * compile-time code strings — no user input is ever rendered here.
 *
 * While the highlighter is still loading (or if it fails), the raw
 * `code` falls back into a `<pre>`.
 */
export function CodePanel({ filePath, code, maxHeight, lang = 'cobol' }: CodePanelProps) {
  const html = useShikiHtml(code, lang);

  return (
    <div
      className="code-panel"
      style={{
        display: 'flex',
        flexDirection: 'column',
        width: '100%',
      }}
    >
      {/* Header bar */}
      <div
        style={{
          display: 'flex',
          alignItems: 'center',
          padding: '12px 16px',
          borderBottom: '1px solid var(--color-bg-code-border)',
          background: 'rgba(7, 11, 24, 0.6)',
        }}
      >
        <div style={{ display: 'flex', gap: 8 }}>
          <span
            style={{
              width: 12,
              height: 12,
              borderRadius: '50%',
              background: '#FF5F56',
              boxShadow: 'inset 0 0 0 1px rgba(0,0,0,0.3)',
            }}
          />
          <span
            style={{
              width: 12,
              height: 12,
              borderRadius: '50%',
              background: '#FFBD2E',
              boxShadow: 'inset 0 0 0 1px rgba(0,0,0,0.3)',
            }}
          />
          <span
            style={{
              width: 12,
              height: 12,
              borderRadius: '50%',
              background: '#27C93F',
              boxShadow: 'inset 0 0 0 1px rgba(0,0,0,0.3)',
            }}
          />
        </div>
        <div
          style={{
            flex: 1,
            textAlign: 'center',
            fontFamily: 'var(--font-mono)',
            fontSize: 13,
            color: 'var(--color-text-muted)',
            letterSpacing: '0.02em',
          }}
        >
          {filePath}
        </div>
        {/* Right-side spacer to keep filePath visually centered */}
        <div style={{ width: 52 }} />
      </div>

      {/* Body */}
      <div
        style={{
          maxHeight: maxHeight ? `${maxHeight}px` : undefined,
          overflow: maxHeight ? 'auto' : 'visible',
        }}
      >
        {html ? (
          <div dangerouslySetInnerHTML={{ __html: html }} />
        ) : (
          <pre>
            <code>{code}</code>
          </pre>
        )}
      </div>
    </div>
  );
}

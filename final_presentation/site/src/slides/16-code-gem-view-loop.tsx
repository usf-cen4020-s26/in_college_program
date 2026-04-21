import type { CSSProperties, ReactNode } from 'react';
import { SlideFrame } from '../components/SlideFrame';
import { CodePanel } from '../components/CodePanel';
import { SNIPPETS } from '../data/snippets';
import type { SlideProps } from './types';

const codeInline: CSSProperties = {
  fontFamily: 'var(--font-mono)',
  fontSize: 14,
  color: 'var(--color-brand-accent)',
  background: 'rgba(10,102,194,0.16)',
  padding: '2px 6px',
  borderRadius: 4,
  border: '1px solid rgba(112,181,249,0.18)',
};

const CALLOUTS: readonly { index: string; body: ReactNode }[] = [
  {
    index: '01',
    body: (
      <>
        <code style={codeInline}>AT END</code> flags the eof sentinel — classic
        COBOL file-status pattern.
      </>
    ),
  },
  {
    index: '02',
    body: (
      <>
        Recipient filter lives inline:{' '}
        <code style={codeInline}>
          MSG-RECIPIENT = WS-USERNAME(WS-CURRENT-USER-INDEX)
        </code>{' '}
        — security by construction.
      </>
    ),
  },
  {
    index: '03',
    body: (
      <>
        Tail recurse via{' '}
        <code style={codeInline}>PERFORM 7841-VIEW-MESSAGES-LOOP</code> — no
        loop keyword needed, just the paragraph calling itself.
      </>
    ),
  },
];

export function Slide16CodeGemViewLoop(_props: SlideProps) {
  return (
    <SlideFrame act="ACT III · HOW WE BUILT IT" kicker="CODE GEM · EPIC #9">
      <div>
        <h1
          style={{
            margin: 0,
            fontFamily: 'var(--font-display)',
            fontWeight: 700,
            fontSize: 72,
            lineHeight: 1.05,
            letterSpacing: '-0.02em',
            color: 'var(--color-text-primary)',
          }}
        >
          Recursive reads, the COBOL way.
        </h1>
      </div>

      <div
        style={{
          display: 'grid',
          gridTemplateColumns: '3fr 2fr',
          gap: 48,
          alignItems: 'start',
          flex: 1,
          minHeight: 0,
        }}
      >
        <div style={{ minWidth: 0 }}>
          <CodePanel
            filePath="src/VIEWMESSAGE.cpy:75-120"
            code={SNIPPETS.viewLoop}
            maxHeight={720}
          />
        </div>

        <div style={{ display: 'flex', flexDirection: 'column', gap: 20 }}>
          {CALLOUTS.map((c, i) => (
            <div
              key={c.index}
              style={{
                padding: '24px 26px',
                borderRadius: 18,
                background: 'var(--color-bg-panel)',
                border: '1px solid rgba(112,181,249,0.14)',
                boxShadow:
                  '0 24px 48px -28px rgba(0,0,0,0.75), inset 0 1px 0 rgba(255,255,255,0.04)',
                display: 'flex',
                alignItems: 'flex-start',
                gap: 18,
                animation: `slide16-in 520ms ${200 + i * 120}ms both cubic-bezier(0.16,1,0.3,1)`,
                opacity: 0,
              }}
            >
              <div
                style={{
                  flexShrink: 0,
                  width: 44,
                  height: 44,
                  borderRadius: 12,
                  background: 'rgba(10,102,194,0.22)',
                  border: '1px solid rgba(112,181,249,0.32)',
                  color: 'var(--color-brand-accent)',
                  display: 'flex',
                  alignItems: 'center',
                  justifyContent: 'center',
                  fontFamily: 'var(--font-mono)',
                  fontWeight: 700,
                  fontSize: 16,
                  letterSpacing: '0.04em',
                }}
              >
                {c.index}
              </div>
              <div
                style={{
                  flex: 1,
                  fontFamily: 'var(--font-body)',
                  fontSize: 17,
                  lineHeight: 1.55,
                  color: 'var(--color-text-muted)',
                }}
              >
                {c.body}
              </div>
            </div>
          ))}
        </div>
      </div>

      <div
        style={{
          fontFamily: 'var(--font-body)',
          fontSize: 14,
          color: 'var(--color-text-dim)',
          letterSpacing: '0.02em',
        }}
      >
        45 lines. Zero buffering. Oldest message first. Handles missing
        MESSAGES.DAT.
      </div>

      <style>{`
        @keyframes slide16-in {
          from { opacity: 0; transform: translateY(12px); }
          to { opacity: 1; transform: translateY(0); }
        }
      `}</style>
    </SlideFrame>
  );
}

import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';

/**
 * Slide 25 — The Menu Problem.
 * Central quote + three stat boxes showing menu evolution.
 */
export function Slide25M4MenuProblem(_props: SlideProps) {
  return (
    <SlideFrame act="PART B · BUILDING INCOLLEGE" kicker="PAIN POINT">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(36px, 4em, 64px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          lineHeight: 1.05,
        }}
      >
        <span className="text-gradient">The Menu Problem</span>
      </h1>

      <div
        style={{
          flex: 1,
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'center',
          justifyContent: 'center',
          gap: 48,
        }}
      >
        <blockquote
          style={{
            margin: 0,
            fontFamily: 'var(--font-display)',
            fontSize: 'clamp(24px, 3em, 48px)',
            fontWeight: 400,
            fontStyle: 'italic',
            lineHeight: 1.35,
            color: 'var(--color-text-primary)',
            textAlign: 'center',
            maxWidth: '85%',
          }}
        >
          "Every time the menu changed, we had to update 100+ test files."
        </blockquote>

        <p
          style={{
            margin: 0,
            fontFamily: 'var(--font-mono)',
            fontSize: 'clamp(11px, 0.8em, 14px)',
            letterSpacing: '0.16em',
            textTransform: 'uppercase',
            color: 'var(--color-brand-accent)',
            opacity: 0.78,
            textAlign: 'center',
          }}
        >
          → Park this thought. Interlude B is the fix.
        </p>

        <div
          style={{
            display: 'flex',
            alignItems: 'center',
            gap: 24,
          }}
        >
          {[
            { label: '4 options', sub: 'Epic 1' },
            { label: '7 options', sub: 'Epic 3' },
            { label: '9 options', sub: 'Epic 5' },
          ].map((item, i) => (
            <div key={item.label} style={{ display: 'flex', alignItems: 'center' }}>
              {i > 0 && (
                <span
                  style={{
                    fontFamily: 'var(--font-mono)',
                    fontSize: 'clamp(20px, 2em, 36px)',
                    color: 'var(--color-text-dim)',
                    margin: '0 20px',
                  }}
                >
                  →
                </span>
              )}
              <div
                style={{
                  padding: '28px 36px',
                  borderRadius: 18,
                  background: 'var(--color-bg-card)',
                  border: '1px solid rgba(112,181,249,0.12)',
                  boxShadow:
                    '0 24px 48px -28px rgba(0,0,0,0.75), inset 0 1px 0 rgba(255,255,255,0.04)',
                  textAlign: 'center',
                }}
              >
                <div
                  style={{
                    fontFamily: 'var(--font-mono)',
                    fontSize: 'clamp(24px, 2.2em, 40px)',
                    fontWeight: 700,
                    color: 'var(--color-brand-accent)',
                    lineHeight: 1,
                  }}
                >
                  {item.label}
                </div>
                <div
                  style={{
                    fontFamily: 'var(--font-body)',
                    fontSize: 'clamp(12px, 0.9em, 15px)',
                    color: 'var(--color-text-muted)',
                    marginTop: 8,
                  }}
                >
                  {item.sub}
                </div>
              </div>
            </div>
          ))}
        </div>
      </div>
    </SlideFrame>
  );
}

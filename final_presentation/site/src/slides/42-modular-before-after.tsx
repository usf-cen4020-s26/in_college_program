import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { BeforeAfter } from '../components/BeforeAfter';

const groups: { label: string; count: number; color: string }[] = [
  { label: 'WS', count: 7, color: '#94A3B8' },
  { label: 'Auth', count: 1, color: '#38BDF8' },
  { label: 'Data', count: 1, color: '#0A66C2' },
  { label: 'Profiles', count: 2, color: '#70B5F9' },
  { label: 'Connections', count: 5, color: '#22D3EE' },
  { label: 'Jobs', count: 5, color: '#F59E0B' },
  { label: 'Messaging', count: 2, color: '#A855F7' },
  { label: 'Other', count: 4, color: '#EF4444' },
];

const pillStyle = (color: string) => ({
  display: 'inline-block' as const,
  padding: '4px 12px',
  borderRadius: 999,
  fontSize: 'clamp(11px, 0.8em, 14px)',
  fontFamily: 'var(--font-mono)',
  fontWeight: 600 as const,
  color,
  border: `1px solid ${color}44`,
  background: `${color}18`,
  whiteSpace: 'nowrap' as const,
});

/**
 * Slide 42 — Before vs After: monolith to copybooks.
 */
export function Slide42ModularBA({ step }: SlideProps) {
  return (
    <SlideFrame act="INTERLUDE" kicker="MODULARIZATION · BEFORE & AFTER">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(36px, 4em, 64px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
          lineHeight: 1.05,
        }}
      >
        The Great <span className="text-gradient">Refactor</span>
      </h1>

      <BeforeAfter
        step={step}
        beforeLabel="Before"
        afterLabel="After"
        before={
          <div
            style={{
              display: 'flex',
              flexDirection: 'column',
              alignItems: 'center',
              justifyContent: 'center',
              flex: 1,
              gap: 16,
            }}
          >
            <span
              style={{
                fontFamily: 'var(--font-mono)',
                fontSize: 'clamp(14px, 1.1em, 18px)',
                color: 'var(--color-brand-accent)',
              }}
            >
              main.cob
            </span>
            <span
              style={{
                fontFamily: 'var(--font-mono)',
                fontSize: 'clamp(48px, 5em, 80px)',
                fontWeight: 700,
                color: 'var(--color-state-danger)',
                lineHeight: 1,
              }}
            >
              ~1,950
            </span>
            <span
              style={{
                fontFamily: 'var(--font-body)',
                fontSize: 'clamp(14px, 1em, 18px)',
                color: 'var(--color-text-muted)',
              }}
            >
              lines — one monolithic file
            </span>
          </div>
        }
        after={
          <div
            style={{
              display: 'flex',
              flexDirection: 'column',
              alignItems: 'center',
              gap: 20,
            }}
          >
            {/* Center main.cob */}
            <div
              style={{
                padding: '12px 24px',
                borderRadius: 14,
                background: 'rgba(10,102,194,0.18)',
                border: '1px solid rgba(112,181,249,0.3)',
                textAlign: 'center',
              }}
            >
              <span
                style={{
                  fontFamily: 'var(--font-mono)',
                  fontSize: 'clamp(16px, 1.4em, 24px)',
                  fontWeight: 700,
                  color: 'var(--color-brand-accent)',
                }}
              >
                main.cob
              </span>
              <span
                style={{
                  fontFamily: 'var(--font-mono)',
                  fontSize: 'clamp(12px, 0.9em, 15px)',
                  color: 'var(--color-text-muted)',
                  marginLeft: 8,
                }}
              >
                (388 lines)
              </span>
            </div>

            {/* 27 copybooks label */}
            <span
              style={{
                fontFamily: 'var(--font-body)',
                fontSize: 'clamp(12px, 0.9em, 15px)',
                color: 'var(--color-text-dim)',
                letterSpacing: '0.1em',
                textTransform: 'uppercase',
              }}
            >
              27 copybooks
            </span>

            {/* Pill grid */}
            <div
              style={{
                display: 'flex',
                flexWrap: 'wrap',
                gap: 8,
                justifyContent: 'center',
              }}
            >
              {groups.map((g) => (
                <span key={g.label} style={pillStyle(g.color)}>
                  {g.label} ({g.count})
                </span>
              ))}
            </div>
          </div>
        }
      />
    </SlideFrame>
  );
}

import type { SlideProps } from './types';
import type { StorageStrategy } from '../data/storage';
import { SlideFrame } from '../components/SlideFrame';
import { STORAGE } from '../data/storage';

/**
 * "How is data stored?" slide. Seven cards in a 4 + 3 grid, one per
 * persistence file, each showing a strategy chip, a short purpose
 * sentence, a collapsed record-layout preview, and the capacity.
 */
type StrategyStyle = {
  label: string;
  color: string;
  background: string;
  border: string;
};

const STRATEGY_STYLES: Record<StorageStrategy, StrategyStyle> = {
  append: {
    label: 'APPEND',
    color: '#BBE8FF',
    background: 'rgba(112,181,249,0.14)',
    border: 'rgba(112,181,249,0.38)',
  },
  rewrite: {
    label: 'REWRITE',
    color: '#FCD9A0',
    background: 'rgba(245,158,11,0.14)',
    border: 'rgba(245,158,11,0.38)',
  },
  hybrid: {
    label: 'HYBRID',
    color: '#BAE6FD',
    background: 'rgba(56,189,248,0.14)',
    border: 'rgba(56,189,248,0.38)',
  },
};

const PREVIEW_LINES = 3;

export function Slide15DataStorage(_props: SlideProps) {
  return (
    <SlideFrame act="ACT III · HOW WE BUILT IT" kicker="PERSISTENCE">
      <style>{`
        @keyframes slide15-headline {
          0% { opacity: 0; transform: translateY(20px); }
          100% { opacity: 1; transform: translateY(0); }
        }
        @keyframes slide15-card {
          0% { opacity: 0; transform: translateY(28px) scale(0.98); }
          100% { opacity: 1; transform: translateY(0) scale(1); }
        }
        .slide15-headline {
          animation: slide15-headline 900ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 80ms;
        }
        .slide15-sub {
          animation: slide15-headline 1000ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 200ms;
        }
        .slide15-card {
          animation: slide15-card 900ms cubic-bezier(0.16, 1, 0.3, 1) both;
        }
      `}</style>

      <div>
        <h1
          className="slide15-headline"
          style={{
            margin: 0,
            fontFamily: 'var(--font-display)',
            fontWeight: 700,
            fontSize: 72,
            letterSpacing: '-0.025em',
            lineHeight: 1.02,
            color: 'var(--color-text-primary)',
          }}
        >
          <span className="text-gradient">Seven files.</span> All hand-written.
        </h1>
        <p
          className="slide15-sub"
          style={{
            margin: '18px 0 0',
            fontFamily: 'var(--font-body)',
            fontSize: 20,
            color: 'var(--color-text-muted)',
            lineHeight: 1.5,
            maxWidth: 1400,
          }}
        >
          Fixed-width sequential records. Loaded into memory on startup.
          Rewritten or appended per file based on access pattern.
        </p>
      </div>

      <div
        style={{
          flex: 1,
          display: 'grid',
          gridTemplateColumns: 'repeat(4, minmax(0, 1fr))',
          gap: 22,
          alignContent: 'start',
        }}
      >
        {STORAGE.map((file, i) => {
          const strategy = STRATEGY_STYLES[file.strategy];
          const visibleLayout = file.recordLayout.slice(0, PREVIEW_LINES);
          const hiddenCount = file.recordLayout.length - visibleLayout.length;

          return (
            <div
              key={file.name}
              className="slide15-card"
              style={{
                animationDelay: `${260 + i * 90}ms`,
                background: 'var(--color-bg-panel)',
                border: '1px solid rgba(112,181,249,0.14)',
                borderRadius: 18,
                padding: '20px 22px 20px',
                display: 'flex',
                flexDirection: 'column',
                gap: 14,
                boxShadow:
                  '0 24px 48px -30px rgba(0,0,0,0.85), inset 0 1px 0 rgba(255,255,255,0.04)',
                minWidth: 0,
              }}
            >
              <div
                style={{
                  display: 'flex',
                  alignItems: 'center',
                  justifyContent: 'space-between',
                  gap: 10,
                }}
              >
                <div
                  style={{
                    fontFamily: 'var(--font-mono)',
                    fontWeight: 700,
                    fontSize: 18,
                    color: 'var(--color-text-primary)',
                    letterSpacing: '0.01em',
                    overflow: 'hidden',
                    textOverflow: 'ellipsis',
                    whiteSpace: 'nowrap',
                  }}
                >
                  {file.name}
                </div>
                <span
                  style={{
                    flexShrink: 0,
                    padding: '4px 10px',
                    borderRadius: 999,
                    fontFamily: 'var(--font-body)',
                    fontWeight: 700,
                    fontSize: 10,
                    letterSpacing: '0.18em',
                    color: strategy.color,
                    background: strategy.background,
                    border: `1px solid ${strategy.border}`,
                  }}
                >
                  {strategy.label}
                </span>
              </div>

              <p
                style={{
                  margin: 0,
                  fontFamily: 'var(--font-body)',
                  fontSize: 14,
                  lineHeight: 1.5,
                  color: 'var(--color-text-muted)',
                  minHeight: 62,
                }}
              >
                {file.purpose}
              </p>

              <div
                style={{
                  background: 'var(--color-bg-code)',
                  border: '1px solid var(--color-bg-code-border)',
                  borderRadius: 10,
                  padding: '12px 14px',
                  fontFamily: 'var(--font-mono)',
                  fontSize: 11,
                  lineHeight: 1.65,
                  color: 'var(--color-text-muted)',
                  overflow: 'hidden',
                }}
              >
                {visibleLayout.map((line) => (
                  <div
                    key={line}
                    style={{
                      whiteSpace: 'nowrap',
                      overflow: 'hidden',
                      textOverflow: 'ellipsis',
                    }}
                  >
                    {line}
                  </div>
                ))}
                {hiddenCount > 0 && (
                  <div
                    style={{
                      color: 'var(--color-text-dim)',
                      letterSpacing: '0.1em',
                    }}
                  >
                    … +{hiddenCount} more
                  </div>
                )}
              </div>

              <div
                style={{
                  marginTop: 'auto',
                  paddingTop: 4,
                  fontFamily: 'var(--font-body)',
                  fontSize: 12,
                  color: 'var(--color-text-dim)',
                  letterSpacing: '0.04em',
                  textTransform: 'uppercase',
                }}
              >
                {file.capacity}
              </div>
            </div>
          );
        })}
      </div>
    </SlideFrame>
  );
}

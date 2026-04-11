import type { ReactNode } from 'react';

interface BeforeAfterProps {
  step: number;
  beforeLabel?: string;
  afterLabel?: string;
  before: ReactNode;
  after: ReactNode;
}

/**
 * Two-panel side-by-side comparison with animated reveal.
 * Step 0 shows the "before" panel. Step 1+ reveals the "after" panel.
 */
export function BeforeAfter({
  step,
  beforeLabel = 'Before',
  afterLabel = 'After',
  before,
  after,
}: BeforeAfterProps) {
  const showAfter = step >= 1;
  return (
    <div
      style={{
        display: 'grid',
        gridTemplateColumns: '1fr 1fr',
        gap: 'var(--spacing-gap)',
        flex: 1,
        minHeight: 0,
      }}
    >
      {/* Before panel */}
      <div
        style={{
          display: 'flex',
          flexDirection: 'column',
          gap: 12,
          transition: 'opacity 400ms ease',
          opacity: showAfter ? 0.5 : 1,
        }}
      >
        <span
          style={{
            fontFamily: 'var(--font-body)',
            fontSize: 'clamp(10px, 0.8em, 13px)',
            letterSpacing: '0.2em',
            textTransform: 'uppercase',
            color: 'var(--color-state-danger)',
            fontWeight: 600,
          }}
        >
          {beforeLabel}
        </span>
        <div
          style={{
            flex: 1,
            borderRadius: 16,
            background: 'var(--color-bg-card)',
            border: '1px solid rgba(133,197,255,0.08)',
            padding: 'clamp(16px, 1.5em, 28px)',
            overflow: 'auto',
          }}
        >
          {before}
        </div>
      </div>

      {/* After panel */}
      <div
        className={`step-reveal${showAfter ? ' visible' : ''}`}
        style={{
          display: 'flex',
          flexDirection: 'column',
          gap: 12,
        }}
      >
        <span
          style={{
            fontFamily: 'var(--font-body)',
            fontSize: 'clamp(10px, 0.8em, 13px)',
            letterSpacing: '0.2em',
            textTransform: 'uppercase',
            color: 'var(--color-state-success)',
            fontWeight: 600,
          }}
        >
          {afterLabel}
        </span>
        <div
          style={{
            flex: 1,
            borderRadius: 16,
            background: 'var(--color-bg-card)',
            border: '1px solid rgba(34,197,94,0.15)',
            padding: 'clamp(16px, 1.5em, 28px)',
            overflow: 'auto',
          }}
        >
          {after}
        </div>
      </div>
    </div>
  );
}

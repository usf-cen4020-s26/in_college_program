import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';

const boxes: { label: string; epic: string; color: string }[] = [
  { label: '4 options', epic: 'Epic 1', color: '#22c55e' },
  { label: '7 options', epic: 'Epic 4', color: '#eab308' },
  { label: '9 options', epic: 'Epic 8', color: '#ef4444' },
];

/**
 * Slide 30 — The Problem: menu changes break everything.
 */
export function Slide30MacroProblem({ step }: SlideProps) {
  return (
    <SlideFrame act="INTERLUDE" kicker="MACROS & SEEDS · THE PROBLEM">
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
        Menu Changes <span className="text-gradient">Break Everything</span>
      </h1>

      <div
        style={{
          display: 'flex',
          gap: 'clamp(16px, 2em, 40px)',
          justifyContent: 'center',
          flex: 1,
          alignItems: 'center',
        }}
      >
        {boxes.map((box) => (
          <div
            key={box.epic}
            style={{
              width: 'clamp(180px, 20%, 260px)',
              padding: 'clamp(28px, 2.5em, 44px) clamp(20px, 1.5em, 32px)',
              borderRadius: 20,
              background: 'var(--color-bg-card)',
              border: `2px solid ${box.color}`,
              display: 'flex',
              flexDirection: 'column',
              alignItems: 'center',
              gap: 12,
            }}
          >
            <span
              style={{
                fontFamily: 'var(--font-mono)',
                fontSize: 'clamp(32px, 3.5em, 56px)',
                fontWeight: 700,
                color: box.color,
                lineHeight: 1,
              }}
            >
              {box.label}
            </span>
            <span
              style={{
                fontFamily: 'var(--font-body)',
                fontSize: 'clamp(13px, 1em, 16px)',
                color: 'var(--color-text-muted)',
                letterSpacing: '0.12em',
                textTransform: 'uppercase',
              }}
            >
              {box.epic}
            </span>
          </div>
        ))}
      </div>

      <StepReveal currentStep={step} visibleAt={1}>
        <p
          style={{
            margin: 0,
            fontFamily: 'var(--font-body)',
            fontSize: 'clamp(16px, 1.25em, 22px)',
            color: 'var(--color-text-muted)',
            lineHeight: 1.5,
            textAlign: 'center',
          }}
        >
          Hundreds of{' '}
          <span style={{ fontFamily: 'var(--font-mono)', color: 'var(--color-brand-accent)' }}>
            .out.txt
          </span>{' '}
          files contained the menu text. Every change meant updating them all by hand.
        </p>
      </StepReveal>
    </SlideFrame>
  );
}

import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';

const FLOW_ITEMS = [
  'test_runner.py',
  'discovers fixtures',
  'runs COBOL binary',
  'compares output',
  'PASS / FAIL',
];

/**
 * Slide 19 — Test Runner Phase 1: Birth.
 * Simple flow diagram. Step 0: first 3 boxes. Step 1: comparison + result.
 */
export function Slide19TRBirth({ step }: SlideProps) {
  return (
    <SlideFrame act="INTERLUDE" kicker="TEST RUNNER · PHASE 1">
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
        Birth
      </h1>

      <div
        style={{
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          gap: 0,
          flex: 1,
          flexWrap: 'wrap',
        }}
      >
        {FLOW_ITEMS.map((item, i) => {
          const visibleAt = i < 3 ? 0 : 1;
          return (
            <StepReveal key={item} currentStep={step} visibleAt={visibleAt}>
              <div style={{ display: 'flex', alignItems: 'center' }}>
                {i > 0 && (
                  <span
                    style={{
                      fontFamily: 'var(--font-mono)',
                      fontSize: 'clamp(20px, 2em, 36px)',
                      color: 'var(--color-text-dim)',
                      margin: '0 16px',
                    }}
                  >
                    →
                  </span>
                )}
                <div
                  style={{
                    padding: '20px 28px',
                    borderRadius: 16,
                    background: 'var(--color-bg-card)',
                    border: '1px solid rgba(112,181,249,0.12)',
                    boxShadow:
                      '0 20px 40px -24px rgba(0,0,0,0.75), inset 0 1px 0 rgba(255,255,255,0.04)',
                    fontFamily: 'var(--font-mono)',
                    fontSize: 'clamp(14px, 1.15em, 20px)',
                    color: i === 4 ? 'var(--color-brand-accent)' : 'var(--color-text-primary)',
                    fontWeight: i === 0 || i === 4 ? 700 : 400,
                    whiteSpace: 'nowrap',
                  }}
                >
                  {item}
                </div>
              </div>
            </StepReveal>
          );
        })}
      </div>
    </SlideFrame>
  );
}

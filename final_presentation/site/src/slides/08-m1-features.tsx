import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';

const COL_1 = [
  'DevContainer & CI pipeline',
  'Core COBOL program structure',
  'Account creation with validation',
];

const COL_2 = [
  'Login with credential check',
  'Max 5 accounts enforced',
  'ACCOUNTS.DAT persistence',
];

/**
 * Slide 08 — What We Built (Epic 1).
 * Two-column bullet list revealed in steps.
 */
export function Slide08M1Features({ step }: SlideProps) {
  return (
    <SlideFrame act="PART B · BUILDING INCOLLEGE" kicker="EPIC #1 · AUTHENTICATION">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(48px, 5.5em, 88px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          lineHeight: 1.05,
        }}
      >
        <span className="text-gradient">What We Built</span>
      </h1>

      <div
        style={{
          display: 'grid',
          gridTemplateColumns: '1fr 1fr',
          gap: 48,
          flex: 1,
          alignItems: 'start',
        }}
      >
        {/* Column 1 — visible at step 0 */}
        <StepReveal currentStep={step} visibleAt={0}>
          <ul
            style={{
              margin: 0,
              padding: 0,
              listStyle: 'none',
              display: 'flex',
              flexDirection: 'column',
              gap: 28,
            }}
          >
            {COL_1.map((item) => (
              <li
                key={item}
                style={{
                  fontFamily: 'var(--font-body)',
                  fontSize: 'clamp(18px, 1.6em, 28px)',
                  color: 'var(--color-text-primary)',
                  lineHeight: 1.4,
                  paddingLeft: 28,
                  position: 'relative',
                }}
              >
                <span
                  style={{
                    position: 'absolute',
                    left: 0,
                    color: 'var(--color-brand-accent)',
                  }}
                >
                  ▸
                </span>
                {item}
              </li>
            ))}
          </ul>
        </StepReveal>

        {/* Column 2 — visible at step 1 */}
        <StepReveal currentStep={step} visibleAt={1}>
          <ul
            style={{
              margin: 0,
              padding: 0,
              listStyle: 'none',
              display: 'flex',
              flexDirection: 'column',
              gap: 28,
            }}
          >
            {COL_2.map((item) => (
              <li
                key={item}
                style={{
                  fontFamily: 'var(--font-body)',
                  fontSize: 'clamp(18px, 1.6em, 28px)',
                  color: 'var(--color-text-primary)',
                  lineHeight: 1.4,
                  paddingLeft: 28,
                  position: 'relative',
                }}
              >
                <span
                  style={{
                    position: 'absolute',
                    left: 0,
                    color: 'var(--color-brand-accent)',
                  }}
                >
                  ▸
                </span>
                {item}
              </li>
            ))}
          </ul>
        </StepReveal>
      </div>
    </SlideFrame>
  );
}

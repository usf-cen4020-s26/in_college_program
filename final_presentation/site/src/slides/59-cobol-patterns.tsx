import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';

const PATTERNS = [
  {
    title: 'Input Pushback',
    description: 'WS-INPUT-PUSHBACK-LINE acts as a mini lookahead buffer, letting the parser "un-read" a line.',
    step: 0,
  },
  {
    title: 'Recursive Read Loops',
    description: 'PERFORM paragraph from within itself until EOF — COBOL\'s answer to while-loops.',
    step: 1,
  },
  {
    title: 'In-Memory Tables',
    description: 'Fixed-size arrays with OCCURS, linear search via PERFORM VARYING — no dynamic allocation needed.',
    step: 2,
  },
  {
    title: 'Dual I/O Channel',
    description: 'Every DISPLAY goes to screen + OUTPUT.TXT, every ACCEPT reads from INPUT.TXT — enabling full test automation.',
    step: 3,
  },
];

/**
 * Slide 59 — Key COBOL Patterns.
 */
export function Slide59Patterns({ step }: SlideProps) {
  return (
    <SlideFrame act="PART D · ARCHITECTURE" kicker="PATTERNS">
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
        <span className="text-gradient">Key COBOL Patterns</span>
      </h1>

      <div
        style={{
          flex: 1,
          display: 'grid',
          gridTemplateColumns: '1fr 1fr',
          gap: 24,
          alignContent: 'center',
        }}
      >
        {PATTERNS.map((p) => (
          <StepReveal key={p.title} currentStep={step} visibleAt={p.step}>
            <div
              style={{
                padding: '28px 30px',
                borderRadius: 18,
                background: 'var(--color-bg-panel)',
                border: '1px solid rgba(112,181,249,0.12)',
                boxShadow:
                  '0 24px 48px -28px rgba(0,0,0,0.75), inset 0 1px 0 rgba(255,255,255,0.04)',
                display: 'flex',
                flexDirection: 'column',
                gap: 14,
              }}
            >
              <div
                style={{
                  fontFamily: 'var(--font-display)',
                  fontWeight: 700,
                  fontSize: 'clamp(18px, 1.5em, 26px)',
                  color: 'var(--color-text-primary)',
                  letterSpacing: '-0.01em',
                }}
              >
                {p.title}
              </div>
              <p
                style={{
                  margin: 0,
                  fontFamily: 'var(--font-body)',
                  fontSize: 'clamp(14px, 1.05em, 18px)',
                  lineHeight: 1.55,
                  color: 'var(--color-text-muted)',
                }}
              >
                {p.description}
              </p>
            </div>
          </StepReveal>
        ))}
      </div>
    </SlideFrame>
  );
}

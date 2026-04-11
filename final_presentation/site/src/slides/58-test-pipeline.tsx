import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';

const STEPS_ROW_1 = [
  { label: 'Fixture', sub: '.in.txt + .out.txt' },
  { label: 'Preprocess', sub: 'seeds + comments' },
  { label: 'Write .DAT files', sub: 'seed data' },
  { label: 'Execute COBOL', sub: 'bin/main' },
];

const STEPS_ROW_2 = [
  { label: 'Expand {{macros}}', sub: 'timestamps, etc.' },
  { label: 'Line-by-line diff', sub: 'expected vs actual' },
  { label: 'PASS / FAIL', sub: 'report result' },
];

const BOX_STYLE: React.CSSProperties = {
  padding: '18px 22px',
  borderRadius: 14,
  background: 'var(--color-bg-panel)',
  border: '1px solid rgba(112,181,249,0.14)',
  boxShadow: '0 12px 32px -16px rgba(0,0,0,0.6)',
  display: 'flex',
  flexDirection: 'column' as const,
  alignItems: 'center',
  gap: 6,
  flex: '1 1 0',
  minWidth: 0,
};

const ARROW: React.CSSProperties = {
  fontSize: 24,
  color: 'var(--color-brand-accent)',
  lineHeight: 1,
  flexShrink: 0,
};

/**
 * Slide 58 — Testing Pipeline: How Every Test Runs.
 */
export function Slide58Pipeline({ step }: SlideProps) {
  return (
    <SlideFrame act="PART D · ARCHITECTURE" kicker="TEST INFRASTRUCTURE">
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
        <span className="text-gradient">How Every Test Runs</span>
      </h1>

      <div
        style={{
          flex: 1,
          display: 'flex',
          flexDirection: 'column',
          justifyContent: 'center',
          gap: 32,
        }}
      >
        <StepReveal currentStep={step} visibleAt={0}>
          <div style={{ display: 'flex', alignItems: 'center', gap: 16 }}>
            {STEPS_ROW_1.map((s, i) => (
              <div key={s.label} style={{ display: 'contents' }}>
                {i > 0 && <span style={ARROW}>&#8594;</span>}
                <div style={BOX_STYLE}>
                  <span
                    style={{
                      fontFamily: 'var(--font-display)',
                      fontWeight: 700,
                      fontSize: 'clamp(14px, 1.1em, 18px)',
                      color: 'var(--color-text-primary)',
                      textAlign: 'center',
                    }}
                  >
                    {s.label}
                  </span>
                  <span
                    style={{
                      fontFamily: 'var(--font-mono)',
                      fontSize: 'clamp(11px, 0.8em, 13px)',
                      color: 'var(--color-text-muted)',
                    }}
                  >
                    {s.sub}
                  </span>
                </div>
              </div>
            ))}
          </div>
        </StepReveal>

        <StepReveal currentStep={step} visibleAt={1}>
          <div style={{ display: 'flex', alignItems: 'center', gap: 16 }}>
            {STEPS_ROW_2.map((s, i) => (
              <div key={s.label} style={{ display: 'contents' }}>
                {i > 0 && <span style={ARROW}>&#8594;</span>}
                <div style={BOX_STYLE}>
                  <span
                    style={{
                      fontFamily: 'var(--font-display)',
                      fontWeight: 700,
                      fontSize: 'clamp(14px, 1.1em, 18px)',
                      color: 'var(--color-text-primary)',
                      textAlign: 'center',
                    }}
                  >
                    {s.label}
                  </span>
                  <span
                    style={{
                      fontFamily: 'var(--font-mono)',
                      fontSize: 'clamp(11px, 0.8em, 13px)',
                      color: 'var(--color-text-muted)',
                    }}
                  >
                    {s.sub}
                  </span>
                </div>
              </div>
            ))}
          </div>
        </StepReveal>
      </div>
    </SlideFrame>
  );
}

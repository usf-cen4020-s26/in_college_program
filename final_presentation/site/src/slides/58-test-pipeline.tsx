import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';

const STEPS = [
  { label: 'Fixture', sub: '.in.txt + .out.txt' },
  { label: 'Preprocess', sub: 'seeds + comments' },
  { label: 'Write .DAT', sub: 'seed data' },
  { label: 'Execute COBOL', sub: 'bin/main' },
  { label: 'Expand macros', sub: 'timestamps, etc.' },
  { label: 'Diff', sub: 'expected vs actual' },
  { label: 'PASS / FAIL', sub: 'report result' },
];

const BOX_STYLE: React.CSSProperties = {
  padding: '14px 12px',
  borderRadius: 12,
  background: 'var(--color-bg-panel)',
  border: '1px solid rgba(112,181,249,0.14)',
  boxShadow: '0 10px 28px -16px rgba(0,0,0,0.6)',
  display: 'flex',
  flexDirection: 'column',
  alignItems: 'center',
  justifyContent: 'center',
  gap: 4,
  flex: '1 1 0',
  minWidth: 0,
  textAlign: 'center',
};

const ARROW: React.CSSProperties = {
  fontSize: 20,
  color: 'var(--color-brand-accent)',
  lineHeight: 1,
  flexShrink: 0,
};

/**
 * Slide 58 — Testing Pipeline: How Every Test Runs.
 * Single horizontal flow so the 7-step pipeline never overflows the page.
 */
export function Slide58Pipeline({ step }: SlideProps) {
  return (
    <SlideFrame act="PART D · ARCHITECTURE" kicker="TEST INFRASTRUCTURE">
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
        <span className="text-gradient">How Every Test Runs</span>
      </h1>

      <div
        style={{
          flex: 1,
          display: 'flex',
          flexDirection: 'column',
          justifyContent: 'center',
          alignItems: 'center',
          gap: 24,
          minHeight: 0,
        }}
      >
        <StepReveal currentStep={step} visibleAt={0}>
          <div
            style={{
              display: 'flex',
              alignItems: 'stretch',
              gap: 8,
              width: '100%',
            }}
          >
            {STEPS.map((s, i) => (
              <div key={s.label} style={{ display: 'contents' }}>
                {i > 0 && (
                  <span
                    style={{
                      ...ARROW,
                      display: 'flex',
                      alignItems: 'center',
                    }}
                  >
                    &#8594;
                  </span>
                )}
                <div style={BOX_STYLE}>
                  <span
                    style={{
                      fontFamily: 'var(--font-display)',
                      fontWeight: 700,
                      fontSize: 'clamp(12px, 0.95em, 16px)',
                      color: 'var(--color-text-primary)',
                    }}
                  >
                    {s.label}
                  </span>
                  <span
                    style={{
                      fontFamily: 'var(--font-mono)',
                      fontSize: 'clamp(10px, 0.72em, 12px)',
                      color: 'var(--color-text-muted)',
                      lineHeight: 1.25,
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
          <p
            style={{
              margin: 0,
              fontFamily: 'var(--font-body)',
              fontSize: 'clamp(14px, 1em, 18px)',
              color: 'var(--color-text-muted)',
              textAlign: 'center',
              maxWidth: '72ch',
              lineHeight: 1.55,
            }}
          >
            Every fixture walks the same seven stages. The runner parses seed
            directives, seeds <code>.DAT</code> files, runs the COBOL binary,
            expands macros in the expected output, diffs line-by-line, and
            reports the verdict — deterministic end-to-end.
          </p>
        </StepReveal>
      </div>
    </SlideFrame>
  );
}

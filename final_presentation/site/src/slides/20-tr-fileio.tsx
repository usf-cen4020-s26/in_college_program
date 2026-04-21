import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';
import { CodePanel } from '../components/CodePanel';

const DIFF_EXAMPLE = `- Enter your choice::
+ Enter your choice:
  1. Create/Edit My Profile
  2. View My Profile`;

/**
 * Slide 20 — Test Runner Phase 2: File-Based I/O.
 * Step 0: flow diagram. Step 1: diff example.
 */
export function Slide20TRFileIO({ step }: SlideProps) {
  return (
    <SlideFrame act="INTERLUDE" kicker="TEST RUNNER · PHASE 2">
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
        File-Based I/O
      </h1>

      <div
        style={{
          display: 'flex',
          flexDirection: 'column',
          gap: 40,
          flex: 1,
          minHeight: 0,
        }}
      >
        {/* Flow diagram */}
        <StepReveal currentStep={step} visibleAt={0}>
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
              gap: 0,
            }}
          >
            {['INPUT.TXT', 'bin/main', 'OUTPUT.TXT'].map((item, i) => (
              <div key={item} style={{ display: 'flex', alignItems: 'center' }}>
                {i > 0 && (
                  <span
                    style={{
                      fontFamily: 'var(--font-mono)',
                      fontSize: 'clamp(20px, 2em, 36px)',
                      color: 'var(--color-text-dim)',
                      margin: '0 24px',
                    }}
                  >
                    →
                  </span>
                )}
                <div
                  style={{
                    padding: '24px 36px',
                    borderRadius: 16,
                    background: 'var(--color-bg-card)',
                    border: '1px solid rgba(112,181,249,0.12)',
                    boxShadow:
                      '0 20px 40px -24px rgba(0,0,0,0.75), inset 0 1px 0 rgba(255,255,255,0.04)',
                    fontFamily: 'var(--font-mono)',
                    fontSize: 'clamp(16px, 1.4em, 24px)',
                    fontWeight: 700,
                    color: i === 1 ? 'var(--color-brand-accent)' : 'var(--color-text-primary)',
                    whiteSpace: 'nowrap',
                  }}
                >
                  {item}
                </div>
              </div>
            ))}
          </div>
        </StepReveal>

        {/* Diff example */}
        <StepReveal currentStep={step} visibleAt={1}>
          <CodePanel
            filePath="unified diff output"
            code={DIFF_EXAMPLE}
            lang="diff"
          />
        </StepReveal>
      </div>
    </SlideFrame>
  );
}

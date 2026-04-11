import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';
import { StatCard } from '../components/StatCard';

const TEST_EXAMPLE = `1        # Login
alice    # Username
Alice1!  # Password
9        # Logout`;

/**
 * Slide 11 — Testing Begins.
 * Step 0: stat "18" + explanation. Step 1: example test file.
 */
export function Slide11M1Testing({ step }: SlideProps) {
  return (
    <SlideFrame act="PART B · BUILDING INCOLLEGE" kicker="EPIC #1 · TESTING">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(48px, 5.5em, 88px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
          lineHeight: 1.05,
        }}
      >
        Testing Begins
      </h1>

      <div
        style={{
          display: 'grid',
          gridTemplateColumns: '1fr 1fr',
          gap: 48,
          flex: 1,
          minHeight: 0,
          alignItems: 'center',
        }}
      >
        {/* Left: stat + explanation */}
        <div style={{ display: 'flex', flexDirection: 'column', gap: 28 }}>
          <StatCard value="18" label="test fixtures" />
          <p
            style={{
              margin: 0,
              fontFamily: 'var(--font-body)',
              fontSize: 'clamp(14px, 1.1em, 20px)',
              lineHeight: 1.55,
              color: 'var(--color-text-muted)',
            }}
          >
            .in.txt / .out.txt file pairs — input piped into COBOL binary,
            output compared line-by-line.
          </p>
        </div>

        {/* Right: example test file */}
        <StepReveal currentStep={step} visibleAt={1}>
          <div
            style={{
              borderRadius: 16,
              background: 'var(--color-bg-card)',
              border: '1px solid rgba(133,197,255,0.08)',
              padding: 'clamp(20px, 1.8em, 32px)',
            }}
          >
            <div
              style={{
                fontFamily: 'var(--font-mono)',
                fontSize: 'clamp(11px, 0.85em, 14px)',
                letterSpacing: '0.2em',
                textTransform: 'uppercase',
                color: 'var(--color-brand-accent)',
                fontWeight: 600,
                marginBottom: 16,
              }}
            >
              login_basic.in.txt
            </div>
            <pre
              style={{
                margin: 0,
                fontFamily: 'var(--font-mono)',
                fontSize: 'clamp(14px, 1.1em, 18px)',
                lineHeight: 1.7,
                color: 'var(--color-text-primary)',
                whiteSpace: 'pre',
              }}
            >
              {TEST_EXAMPLE}
            </pre>
          </div>
        </StepReveal>
      </div>
    </SlideFrame>
  );
}

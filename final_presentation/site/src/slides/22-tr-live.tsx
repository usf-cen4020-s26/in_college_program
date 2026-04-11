import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';

const CLI_COMMANDS = [':help', ':dump', ':show', ':undo', ':rerun', ':quit'];

/**
 * Slide 22 — Test Runner Phases 5-6: Live CLI & Packaging.
 * Step 0: Live CLI commands. Step 1: Packaging description.
 */
export function Slide22TRLive({ step }: SlideProps) {
  return (
    <SlideFrame act="INTERLUDE" kicker="TEST RUNNER · PHASES 5-6">
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
        Live CLI & Packaging
      </h1>

      <div
        style={{
          display: 'grid',
          gridTemplateColumns: '1fr 1fr',
          gap: 48,
          flex: 1,
          minHeight: 0,
          alignItems: 'start',
        }}
      >
        {/* Left: styled terminal showing commands */}
        <StepReveal currentStep={step} visibleAt={0}>
          <div
            style={{
              borderRadius: 20,
              background: 'var(--color-bg-card)',
              border: '1px solid rgba(112,181,249,0.12)',
              boxShadow:
                '0 30px 60px -32px rgba(0,0,0,0.8), inset 0 1px 0 rgba(255,255,255,0.04)',
              padding: '32px 36px',
              display: 'flex',
              flexDirection: 'column',
              gap: 8,
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
                marginBottom: 12,
              }}
            >
              Live CLI
            </div>
            {CLI_COMMANDS.map((cmd) => (
              <div
                key={cmd}
                style={{
                  fontFamily: 'var(--font-mono)',
                  fontSize: 'clamp(16px, 1.4em, 24px)',
                  color: 'var(--color-text-primary)',
                  lineHeight: 1.6,
                  display: 'flex',
                  alignItems: 'center',
                  gap: 12,
                }}
              >
                <span style={{ color: 'var(--color-text-dim)' }}>$</span>
                <span style={{ color: 'var(--color-brand-accent)', fontWeight: 600 }}>
                  {cmd}
                </span>
              </div>
            ))}
          </div>
        </StepReveal>

        {/* Right: packaging description */}
        <StepReveal currentStep={step} visibleAt={1}>
          <div
            style={{
              borderRadius: 20,
              background: 'var(--color-bg-card)',
              border: '1px solid rgba(112,181,249,0.12)',
              boxShadow:
                '0 30px 60px -32px rgba(0,0,0,0.8), inset 0 1px 0 rgba(255,255,255,0.04)',
              padding: '32px 36px',
              display: 'flex',
              flexDirection: 'column',
              gap: 20,
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
              }}
            >
              Packaging
            </div>
            <p
              style={{
                margin: 0,
                fontFamily: 'var(--font-body)',
                fontSize: 'clamp(15px, 1.2em, 20px)',
                lineHeight: 1.6,
                color: 'var(--color-text-muted)',
              }}
            >
              The test runner ships as a standalone Python package with a CLI entry point.
              Teams install it via pip and run tests with a single command — no manual
              setup needed.
            </p>
            <p
              style={{
                margin: 0,
                fontFamily: 'var(--font-body)',
                fontSize: 'clamp(15px, 1.2em, 20px)',
                lineHeight: 1.6,
                color: 'var(--color-text-muted)',
              }}
            >
              Configuration lives in a TOML file alongside the fixtures, keeping
              test definitions version-controlled and reproducible.
            </p>
          </div>
        </StepReveal>
      </div>
    </SlideFrame>
  );
}

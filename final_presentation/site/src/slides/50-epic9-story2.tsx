import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';

const EXAMPLE_OUTPUT = `--------------------
From: alice
Hello Bob!
2026-04-10 14:30:00
--------------------
From: charlie
How's the project?
2026-04-10 15:00:00
--------------------`;

/**
 * Slide 50 — Story 2: Ordering & Formatting.
 */
export function Slide50Story2({ step }: SlideProps) {
  return (
    <SlideFrame act="PART C · EPIC 9 DEEP DIVE" kicker="STORY 2 · FORMATTING">
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
        <span className="text-gradient">Oldest First</span>
      </h1>

      <div
        style={{
          flex: 1,
          display: 'flex',
          flexDirection: 'column',
          gap: 36,
          justifyContent: 'center',
        }}
      >
        <StepReveal currentStep={step} visibleAt={0}>
          <div
            style={{
              padding: '28px 36px',
              borderRadius: 18,
              background: 'var(--color-bg-panel)',
              border: '1px solid rgba(112,181,249,0.12)',
              boxShadow:
                '0 24px 48px -28px rgba(0,0,0,0.75), inset 0 1px 0 rgba(255,255,255,0.04)',
            }}
          >
            <p
              style={{
                margin: 0,
                fontFamily: 'var(--font-body)',
                fontSize: 'clamp(18px, 1.5em, 26px)',
                color: 'var(--color-text-primary)',
                lineHeight: 1.5,
              }}
            >
              Messages appended chronologically{' '}
              <span style={{ color: 'var(--color-brand-accent)' }}>&#8594;</span>{' '}
              natural file order = oldest first.
            </p>
          </div>
        </StepReveal>

        <StepReveal currentStep={step} visibleAt={1}>
          <div
            className="code-panel"
            style={{
              display: 'flex',
              flexDirection: 'column',
              width: '100%',
            }}
          >
            {/* Header bar */}
            <div
              style={{
                display: 'flex',
                alignItems: 'center',
                padding: '12px 16px',
                borderBottom: '1px solid var(--color-bg-code-border)',
                background: 'rgba(7, 11, 24, 0.6)',
              }}
            >
              <div style={{ display: 'flex', gap: 8 }}>
                <span style={{ width: 12, height: 12, borderRadius: '50%', background: '#FF5F56', boxShadow: 'inset 0 0 0 1px rgba(0,0,0,0.3)' }} />
                <span style={{ width: 12, height: 12, borderRadius: '50%', background: '#FFBD2E', boxShadow: 'inset 0 0 0 1px rgba(0,0,0,0.3)' }} />
                <span style={{ width: 12, height: 12, borderRadius: '50%', background: '#27C93F', boxShadow: 'inset 0 0 0 1px rgba(0,0,0,0.3)' }} />
              </div>
              <div
                style={{
                  flex: 1,
                  textAlign: 'center',
                  fontFamily: 'var(--font-mono)',
                  fontSize: 13,
                  color: 'var(--color-text-muted)',
                  letterSpacing: '0.02em',
                }}
              >
                bob@incollege — View Messages
              </div>
              <div style={{ width: 52 }} />
            </div>

            {/* Body */}
            <pre
              style={{
                margin: 0,
                padding: '20px 24px',
                fontFamily: 'var(--font-mono)',
                fontSize: 'clamp(13px, 1em, 17px)',
                lineHeight: 1.6,
                color: 'var(--color-text-primary)',
              }}
            >
              {EXAMPLE_OUTPUT}
            </pre>
          </div>
        </StepReveal>
      </div>
    </SlideFrame>
  );
}

import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';

const BOX_STYLE: React.CSSProperties = {
  padding: '20px 28px',
  borderRadius: 16,
  background: 'var(--color-bg-panel)',
  border: '1px solid rgba(112,181,249,0.14)',
  fontFamily: 'var(--font-body)',
  fontSize: 'clamp(16px, 1.3em, 22px)',
  color: 'var(--color-text-primary)',
  textAlign: 'center' as const,
  boxShadow: '0 12px 32px -16px rgba(0,0,0,0.6)',
  flex: '1 1 0',
};

const ARROW: React.CSSProperties = {
  fontSize: 28,
  color: 'var(--color-brand-accent)',
  lineHeight: 1,
  flexShrink: 0,
};

/**
 * Slide 56 — Program Flow: The Pattern.
 */
export function Slide56ProgramFlow({ step }: SlideProps) {
  return (
    <SlideFrame act="PART D · ARCHITECTURE" kicker="CONTROL FLOW">
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
        <span className="text-gradient">The Pattern</span>
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
          <div style={{ display: 'flex', alignItems: 'center', gap: 20 }}>
            <div style={BOX_STYLE}>Load .DAT files</div>
            <span style={ARROW}>&#8594;</span>
            <div style={BOX_STYLE}>Process input</div>
            <span style={ARROW}>&#8594;</span>
            <div style={BOX_STYLE}>Validate</div>
          </div>
        </StepReveal>

        <StepReveal currentStep={step} visibleAt={1}>
          <div style={{ display: 'flex', alignItems: 'center', gap: 20 }}>
            <div style={BOX_STYLE}>Persist changes</div>
            <span style={ARROW}>&#8594;</span>
            <div style={BOX_STYLE}>Display via 8000-WRITE-OUTPUT</div>
            <span style={ARROW}>&#8594;</span>
            <div style={BOX_STYLE}>Read via 8100-READ-INPUT</div>
          </div>
        </StepReveal>
      </div>
    </SlideFrame>
  );
}

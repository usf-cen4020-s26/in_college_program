import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';
import { CodePanel } from '../components/CodePanel';

const COBOL_CODE = `7840-VIEW-MESSAGES.
    MOVE 0 TO WS-MSG-FOUND
    OPEN INPUT MESSAGES-FILE
    PERFORM 7841-VIEW-MESSAGES-LOOP
    CLOSE MESSAGES-FILE`;

const BOX_STYLE: React.CSSProperties = {
  padding: '12px 18px',
  borderRadius: 12,
  background: 'var(--color-bg-panel)',
  border: '1px solid rgba(112,181,249,0.14)',
  fontFamily: 'var(--font-body)',
  fontSize: 'clamp(14px, 1.1em, 18px)',
  color: 'var(--color-text-primary)',
  textAlign: 'center' as const,
  boxShadow: '0 8px 24px -12px rgba(0,0,0,0.6)',
};

const ARROW: React.CSSProperties = {
  textAlign: 'center' as const,
  fontSize: 22,
  color: 'var(--color-brand-accent)',
  lineHeight: 1,
};

/**
 * Slide 49 — Story 1: Basic View.
 */
export function Slide49Story1({ step }: SlideProps) {
  return (
    <SlideFrame act="PART C · EPIC 9 DEEP DIVE" kicker="STORY 1 · BASIC VIEW">
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
        <span className="text-gradient">View My Messages</span>
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
          <div style={{ display: 'flex', alignItems: 'center', gap: 16, justifyContent: 'center' }}>
            <div style={BOX_STYLE}>Open MESSAGES.DAT</div>
            <span style={ARROW}>&#8594;</span>
            <div style={BOX_STYLE}>Scan records</div>
            <span style={ARROW}>&#8594;</span>
            <div style={BOX_STYLE}>Filter by recipient</div>
            <span style={ARROW}>&#8594;</span>
            <div style={BOX_STYLE}>Display matching</div>
          </div>
        </StepReveal>

        <StepReveal currentStep={step} visibleAt={1}>
          <CodePanel
            filePath="src/MSG.cpy"
            code={COBOL_CODE}
            lang="cobol"
          />
        </StepReveal>
      </div>
    </SlideFrame>
  );
}

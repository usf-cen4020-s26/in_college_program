import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';
import { CodePanel } from '../components/CodePanel';
import { FeatureCallout } from '../components/FeatureCallout';

const COBOL_SNIPPET = `    ADD 1 TO WS-CONN-COUNT
    MOVE WS-CURRENT-USER TO
        WS-CONN-USER-A(WS-CONN-COUNT)
    MOVE WS-REQ-SENDER(WS-REQ-IDX) TO
        WS-CONN-USER-B(WS-CONN-COUNT)
    ADD 1 TO WS-CONN-COUNT
    MOVE WS-REQ-SENDER(WS-REQ-IDX) TO
        WS-CONN-USER-A(WS-CONN-COUNT)
    MOVE WS-CURRENT-USER TO
        WS-CONN-USER-B(WS-CONN-COUNT)`;

/**
 * Slide 27 — Bidirectional Connections.
 * Step 0: CodePanel. Step 1: FeatureCallout explaining the pattern.
 */
export function Slide27M5Bidirectional({ step }: SlideProps) {
  return (
    <SlideFrame act="PART B · BUILDING INCOLLEGE" kicker="EPIC #5 · CONNECTIONS">
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
        Bidirectional Connections
      </h1>

      <div
        style={{
          display: 'flex',
          flexDirection: 'column',
          gap: 32,
          flex: 1,
          minHeight: 0,
        }}
      >
        <StepReveal currentStep={step} visibleAt={0}>
          <CodePanel
            filePath="src/CONNECTIONS.cpy"
            code={COBOL_SNIPPET}
            lang="cobol"
          />
        </StepReveal>

        <StepReveal currentStep={step} visibleAt={1}>
          <FeatureCallout
            title="Symmetric records"
            detail="Both A→B and B→A records are created, so either user can find the other in their network."
            citation="src/CONNECTIONS.cpy"
          />
        </StepReveal>
      </div>
    </SlideFrame>
  );
}

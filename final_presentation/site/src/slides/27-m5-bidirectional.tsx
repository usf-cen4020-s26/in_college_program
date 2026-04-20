import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';
import { CodePanel } from '../components/CodePanel';
import { FeatureCallout } from '../components/FeatureCallout';

const COBOL_SNIPPET = `9400-ADD-CONNECTION.
    MOVE WS-USERNAME(WS-CURRENT-USER-INDEX)
        TO CONN-USER-A
    MOVE WS-VIEWREQ-SENDER-USERNAME
        TO CONN-USER-B
    WRITE CONNECTION-REC
    ADD 1 TO WS-CONNECTIONS-COUNT
    MOVE CONN-USER-A
        TO WS-CONN-USER-A(WS-CONNECTIONS-COUNT)
    MOVE CONN-USER-B
        TO WS-CONN-USER-B(WS-CONNECTIONS-COUNT)`;

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
            title="Single record, bidirectional lookup"
            detail="One A→B record is written. The lookup code checks both A and B slots, so either user finds the other."
            citation="src/CONNWRITE.cpy"
          />
        </StepReveal>
      </div>
    </SlideFrame>
  );
}

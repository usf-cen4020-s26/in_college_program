import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';
import { CodePanel } from '../components/CodePanel';
import { FeatureCallout } from '../components/FeatureCallout';

const COBOL_SNIPPET = `4400-VALIDATE-PASSWORD.
    MOVE 'N' TO WS-PASSWORD-VALID
    IF WS-PASSWORD-LENGTH < 8 OR > 12
        PERFORM 8000-WRITE-OUTPUT
        GO TO 4400-EXIT
    END-IF
    INSPECT WS-NEW-PASSWORD TALLYING
        WS-UPPER-COUNT FOR CHARACTERS
        BEFORE INITIAL 'a'`;

/**
 * Slide 10 — Password Rules.
 * Step 0: CodePanel. Step 1: FeatureCallouts on the right.
 */
export function Slide10M1Password({ step }: SlideProps) {
  return (
    <SlideFrame act="PART B · BUILDING INCOLLEGE" kicker="EPIC #1 · SECURITY">
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
        Password Validation
      </h1>

      <div
        style={{
          display: 'grid',
          gridTemplateColumns: '1.4fr 1fr',
          gap: 40,
          flex: 1,
          minHeight: 0,
          alignItems: 'start',
        }}
      >
        {/* Code panel — always visible */}
        <StepReveal currentStep={step} visibleAt={0}>
          <CodePanel
            filePath="src/AUTH.cpy"
            code={COBOL_SNIPPET}
            lang="cobol"
          />
        </StepReveal>

        {/* Feature callouts — step 1 */}
        <StepReveal currentStep={step} visibleAt={1}>
          <div
            style={{
              display: 'flex',
              flexDirection: 'column',
              gap: 20,
            }}
          >
            <FeatureCallout
              title="8-12 characters"
              detail="Password length is checked before any other validation — too short or too long bounces immediately."
            />
            <FeatureCallout
              title="1 uppercase letter"
              detail="INSPECT TALLYING counts uppercase characters using the BEFORE INITIAL technique."
            />
            <FeatureCallout
              title="1 digit + 1 special char"
              detail="Additional INSPECT passes verify at least one numeric and one non-alphanumeric character."
            />
          </div>
        </StepReveal>
      </div>
    </SlideFrame>
  );
}

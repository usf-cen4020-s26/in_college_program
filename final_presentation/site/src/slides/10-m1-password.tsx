import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';
import { CodePanel } from '../components/CodePanel';
import { FeatureCallout } from '../components/FeatureCallout';

const COBOL_SNIPPET = `4400-VALIDATE-PASSWORD.
    MOVE 0 TO WS-HAS-CAPITAL
    MOVE 0 TO WS-HAS-DIGIT
    MOVE 0 TO WS-HAS-SPECIAL
    IF WS-PASSWORD-LENGTH < 8 OR
       WS-PASSWORD-LENGTH > 12
        EXIT PARAGRAPH
    END-IF
    PERFORM VARYING WS-CHAR-INDEX FROM 1 BY 1
        UNTIL WS-CHAR-INDEX > WS-PASSWORD-LENGTH
        MOVE WS-PASSWORD-INPUT(WS-CHAR-INDEX:1)
            TO WS-CURRENT-CHAR
        IF WS-CURRENT-CHAR >= "A" AND
           WS-CURRENT-CHAR <= "Z"
            MOVE 1 TO WS-HAS-CAPITAL
        END-IF
        IF WS-CURRENT-CHAR >= "0" AND
           WS-CURRENT-CHAR <= "9"
            MOVE 1 TO WS-HAS-DIGIT
        END-IF
        IF WS-CURRENT-CHAR = "!" OR "@"
           OR "#" OR "$" OR "%" OR "^"
           OR "&" OR "*"
            MOVE 1 TO WS-HAS-SPECIAL
        END-IF
    END-PERFORM`;

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
              detail="Character-by-character loop checks A–Z range; sets WS-HAS-CAPITAL on first match."
            />
            <FeatureCallout
              title="1 digit + 1 special char"
              detail="Range check 0–9 sets WS-HAS-DIGIT; explicit OR chain for ! @ # $ % ^ & * sets WS-HAS-SPECIAL."
            />
          </div>
        </StepReveal>
      </div>
    </SlideFrame>
  );
}

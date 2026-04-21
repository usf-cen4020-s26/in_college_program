import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';
import { FeatureCallout } from '../components/FeatureCallout';

/**
 * Slide 57 — Security Features: Defense in Depth.
 */
export function Slide57Security({ step }: SlideProps) {
  return (
    <SlideFrame act="PART D · ARCHITECTURE" kicker="SECURITY">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(28px, 3.5em, 56px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          lineHeight: 1.05,
        }}
      >
        <span className="text-gradient">Defense in Depth</span>
      </h1>

      <div
        style={{
          flex: 1,
          display: 'flex',
          flexDirection: 'column',
          gap: 10,
          justifyContent: 'center',
        }}
      >
        <StepReveal currentStep={step} visibleAt={0}>
          <div style={{ display: 'flex', flexDirection: 'column', gap: 10 }}>
            <FeatureCallout
              title="Password policy"
              detail="8-12 chars, requires uppercase, digit, and special character. Rejects weak passwords instantly."
              citation="src/AUTH.cpy:4400-VALIDATE-PASSWORD"
            />
            <FeatureCallout
              title="Account cap"
              detail="Hard limit of 5 accounts enforced at creation time. Prevents runaway data growth."
              citation="src/WS-CONSTANTS.cpy"
            />
            <FeatureCallout
              title="Connection-only messaging"
              detail="Users can only send messages to accepted connections. No unsolicited messages."
              citation="src/MSG.cpy:7800-SEND-MESSAGE"
            />
          </div>
        </StepReveal>

        <StepReveal currentStep={step} visibleAt={1}>
          <div style={{ display: 'flex', flexDirection: 'column', gap: 10 }}>
            <FeatureCallout
              title="File status checks"
              detail="Every FILE OPEN checks FILE STATUS before proceeding. Graceful handling of missing or corrupt data."
              citation="src/IO.cpy"
            />
            <FeatureCallout
              title="Input validation"
              detail="All user input validated for length, format, and business rules before any write."
              citation="src/VALIDATE.cpy"
            />
          </div>
        </StepReveal>
      </div>
    </SlideFrame>
  );
}

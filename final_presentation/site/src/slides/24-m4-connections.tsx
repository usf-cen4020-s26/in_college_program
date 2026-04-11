import { Player } from '@remotion/player';
import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { FeatureCallout } from '../components/FeatureCallout';
import { StepReveal } from '../components/StepReveal';
import { Terminal } from '../remotion/compositions';
import { connectAcceptScript } from '../data/terminals/connect-accept';
import { scriptDurationFrames } from '../lib/terminalDuration';

const FPS = 30;
const SPEED_FACTOR = 0.5;
const DURATION = scriptDurationFrames(connectAcceptScript, FPS, 60) / SPEED_FACTOR;

/**
 * Slide 24 — Send & View Requests.
 * Left: terminal. Right: FeatureCallouts with step reveals.
 */
export function Slide24M4Connections({ step }: SlideProps) {
  return (
    <SlideFrame act="PART B · BUILDING INCOLLEGE" kicker="EPIC #4 · CONNECTIONS">
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
        Send & View Requests
      </h1>

      <div
        style={{
          display: 'grid',
          gridTemplateColumns: '1.63fr 1fr',
          gap: 48,
          alignItems: 'stretch',
          flex: 1,
          minHeight: 0,
        }}
      >
        <div
          style={{
            borderRadius: 20,
            overflow: 'hidden',
            boxShadow:
              '0 60px 120px -40px rgba(0,0,0,0.8), 0 0 0 1px rgba(112,181,249,0.12)',
          }}
        >
          <Player
            component={Terminal}
            inputProps={{
              script: connectAcceptScript,
              promptLabel: 'incollege $ ',
              title: 'alice@incollege — connect',
              accent: 'brand',
              speedFactor: SPEED_FACTOR,
            }}
            durationInFrames={DURATION}
            fps={FPS}
            compositionWidth={1920}
            compositionHeight={1080}
            autoPlay
            loop
            controls={false}
            clickToPlay={false}
            style={{ width: '100%', height: '100%' }}
          />
        </div>

        <div
          style={{
            display: 'flex',
            flexDirection: 'column',
            gap: 20,
            justifyContent: 'center',
          }}
        >
          <StepReveal currentStep={step} visibleAt={0}>
            <FeatureCallout
              title="Send request"
              detail="Search for a user, then send a connection request — stored in PENDING.DAT until accepted or rejected."
              citation="src/CONNECTIONS.cpy"
            />
          </StepReveal>
          <StepReveal currentStep={step} visibleAt={1}>
            <FeatureCallout
              title="View pending"
              detail="Users see all incoming requests and can accept or reject each one individually."
            />
          </StepReveal>
          <StepReveal currentStep={step} visibleAt={1}>
            <FeatureCallout
              title="Duplicate prevention"
              detail="The system checks both PENDING.DAT and CONNECTIONS.DAT before allowing a new request."
            />
          </StepReveal>
          <StepReveal currentStep={step} visibleAt={1}>
            <div
              style={{
                fontFamily: 'var(--font-mono)',
                fontSize: 'clamp(12px, 0.9em, 15px)',
                color: 'var(--color-text-dim)',
                padding: '12px 16px',
                borderRadius: 10,
                background: 'rgba(112,181,249,0.06)',
                border: '1px solid rgba(112,181,249,0.1)',
              }}
            >
              New file: PENDING.DAT
            </div>
          </StepReveal>
        </div>
      </div>
    </SlideFrame>
  );
}

import { Player } from '@remotion/player';
import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { FeatureCallout } from '../components/FeatureCallout';
import { StepReveal } from '../components/StepReveal';
import { Terminal } from '../remotion/compositions';
import { buildProfileScript } from '../data/terminals/build-profile';
import { scriptDurationFrames } from '../lib/terminalDuration';

const FPS = 30;
const SPEED_FACTOR = 0.5;
const DURATION = scriptDurationFrames(buildProfileScript, FPS, 60) / SPEED_FACTOR;

/**
 * Slide 14 — Profile Features.
 * Left: terminal. Right: FeatureCallouts with step reveals.
 */
export function Slide14M2Features({ step }: SlideProps) {
  return (
    <SlideFrame act="PART B · BUILDING INCOLLEGE" kicker="EPIC #2 · PROFILES">
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
        Build Your Profile
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
              script: buildProfileScript,
              promptLabel: 'incollege $ ',
              title: 'alice@incollege — profile',
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
              title="Create / Edit"
              detail="Users can create a new profile or edit their existing one — all fields re-prompt on blank input."
              citation="src/PROFILE.cpy:7000-CREATE-EDIT-PROFILE"
            />
          </StepReveal>
          <StepReveal currentStep={step} visibleAt={1}>
            <FeatureCallout
              title="About Me"
              detail="Free-text paragraph up to 200 characters that displays on profile view."
              citation="src/WS-PROFILES.cpy"
            />
          </StepReveal>
          <StepReveal currentStep={step} visibleAt={1}>
            <FeatureCallout
              title="Graduation year validation"
              detail="Year must be between 1900 and 2100 — guards against typos and garbage data."
              citation="src/PROFILE.cpy"
            />
          </StepReveal>
        </div>
      </div>
    </SlideFrame>
  );
}

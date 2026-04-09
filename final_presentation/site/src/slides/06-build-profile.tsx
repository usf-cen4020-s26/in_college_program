import { Player } from '@remotion/player';
import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { FeatureCallout } from '../components/FeatureCallout';
import { Terminal } from '../remotion/compositions';
import { buildProfileScript } from '../data/terminals/build-profile';
import { scriptDurationFrames } from '../lib/terminalDuration';

const FPS = 30;
const DURATION_IN_FRAMES = scriptDurationFrames(buildProfileScript, FPS, 60);

/**
 * Slide 06 — Aaron — "Build a profile that actually fits a student".
 *
 * Left: animated terminal walking the Create/Edit Profile flow.
 * Right: three callouts explaining the required fields, optional caps,
 * and persistence across restarts.
 */
export function Slide06BuildProfile(_props: SlideProps) {
  return (
    <SlideFrame act="ACT II · PRODUCT DEMO" kicker="EPIC #2 · PROFILES">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 84,
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
          lineHeight: 1.02,
        }}
      >
        Build a profile that actually{' '}
        <span className="text-gradient">fits a student.</span>
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
            }}
            durationInFrames={DURATION_IN_FRAMES}
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
          <FeatureCallout
            title="Required fields"
            detail="First, Last, University, Major, and Graduation Year all re-prompt on blank — the form refuses to save partial data."
            citation="src/PROFILE.cpy:7000-CREATE-EDIT-PROFILE"
          />
          <FeatureCallout
            title="Bounded history"
            detail="Up to 3 experience entries and 3 education entries per profile — enough for a student, structured enough to render cleanly."
            citation="src/WS-PROFILES.cpy"
          />
          <FeatureCallout
            title="Persisted across restarts"
            detail="Every save rewrites PROFILES.DAT so the next login loads the full profile straight into working storage."
            citation="src/PROFILE.cpy · PROFILES.DAT"
          />
        </div>
      </div>
    </SlideFrame>
  );
}

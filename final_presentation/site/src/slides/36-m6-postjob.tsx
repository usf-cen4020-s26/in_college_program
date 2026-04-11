import { Player } from '@remotion/player';
import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { FeatureCallout } from '../components/FeatureCallout';
import { Terminal } from '../remotion/compositions';
import { postJobScript } from '../data/terminals/post-job';
import { scriptDurationFrames } from '../lib/terminalDuration';

const FPS = 30;
const SPEED_FACTOR = 0.5;
const DURATION = scriptDurationFrames(postJobScript, FPS, 60) / SPEED_FACTOR;

/**
 * Slide 36 — Post a Job: terminal demo + feature callouts.
 */
export function Slide36M6PostJob(_props: SlideProps) {
  return (
    <SlideFrame act="PART B · BUILDING INCOLLEGE" kicker="EPIC #6 · JOBS">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(48px, 5.5em, 88px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
          lineHeight: 1.02,
        }}
      >
        Post a <span className="text-gradient">Job</span>
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
              script: postJobScript,
              promptLabel: 'incollege $ ',
              title: 'alice@incollege — jobs',
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
          <FeatureCallout
            title="Title / Description / Employer / Location"
            detail="All four fields required — blank entries re-prompt until filled."
            citation="src/JOBS.cpy:5100-POST-JOB"
          />
          <FeatureCallout
            title="Optional Salary"
            detail="Enter a salary or type NONE to skip. Stored as-is in JOBS.DAT."
            citation="src/JOBS.cpy:5150"
          />
          <FeatureCallout
            title="Required-Field Re-Prompting"
            detail="Empty inputs loop back to the same prompt with an error message."
          />
          <FeatureCallout
            title="JOBS.DAT Storage (max 25)"
            detail="Jobs persisted to JOBS.DAT with a hard cap of 25 postings."
            citation="src/WS-CONSTANTS.cpy"
          />
        </div>
      </div>
    </SlideFrame>
  );
}

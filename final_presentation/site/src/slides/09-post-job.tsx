import { Player } from '@remotion/player';
import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { FeatureCallout } from '../components/FeatureCallout';
import { Terminal } from '../remotion/compositions';
import { postJobScript } from '../data/terminals/post-job';
import { scriptDurationFrames } from '../lib/terminalDuration';

const FPS = 30;
const DURATION_IN_FRAMES = scriptDurationFrames(postJobScript, FPS, 60);

/**
 * Slide 09 — Victoria — "Post a job".
 *
 * Left: terminal replay of the post-job flow, including the blank-title
 * re-prompt and the NONE-salary skip.
 * Right: three callouts explaining the re-prompting guards, the
 * optional salary path, and the append-only persistence to JOBS.DAT.
 */
export function Slide09PostJob(_props: SlideProps) {
  return (
    <SlideFrame act="ACT II · PRODUCT DEMO" kicker="EPIC #6 · JOBS">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 88,
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
          lineHeight: 1.02,
        }}
      >
        Post a <span className="text-gradient">job.</span>
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
              title: 'carol@incollege — post a job',
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
            title="Required-field re-prompt"
            detail="A blank Job Title is rejected and the prompt loops until a non-empty value is captured — the same guard runs on description, employer, and location."
            citation="src/JOBS.cpy:5101-5160"
          />
          <FeatureCallout
            title="Optional salary"
            detail="Salary is the only optional field. Typing NONE skips it; any other value is stored verbatim and resurfaces in the browse view."
            citation="src/JOBS.cpy:5190-5212"
          />
          <FeatureCallout
            title="Append to JOBS.DAT"
            detail="Accepted postings are appended with an auto-incrementing Job ID, so every prior job stays readable and browseable across restarts."
            citation="src/JOBSIO.cpy · JOBS.DAT"
          />
        </div>
      </div>
    </SlideFrame>
  );
}

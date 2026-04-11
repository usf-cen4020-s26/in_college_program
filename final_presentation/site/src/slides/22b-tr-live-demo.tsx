import { Player } from '@remotion/player';
import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { Terminal } from '../remotion/compositions';
import { liveCLIDemoScript } from '../data/terminals/live-cli-demo';
import { scriptDurationFrames } from '../lib/terminalDuration';

const FPS = 30;
const SPEED_FACTOR = 0.4;
const DURATION = Math.ceil(scriptDurationFrames(liveCLIDemoScript, FPS, 60) / SPEED_FACTOR);

/**
 * Slide 22b — Live CLI Demo.
 * Full-screen terminal showing the test runner's interactive mode.
 */
export function Slide22bTRLiveDemo(_props: SlideProps) {
  return (
    <SlideFrame act="INTERLUDE" kicker="TEST RUNNER · LIVE DEMO">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(36px, 4em, 64px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          lineHeight: 1.05,
        }}
      >
        <span className="text-gradient">Live Mode</span> in Action
      </h1>

      <div
        style={{
          flex: 1,
          minHeight: 0,
          borderRadius: 20,
          overflow: 'hidden',
          boxShadow:
            '0 60px 120px -40px rgba(0,0,0,0.8), 0 0 0 1px rgba(112,181,249,0.12)',
        }}
      >
        <Player
          component={Terminal}
          inputProps={{
            script: liveCLIDemoScript,
            promptLabel: '$ ',
            title: 'incollege-test — live mode',
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
    </SlideFrame>
  );
}

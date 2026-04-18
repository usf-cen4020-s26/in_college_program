import { Player } from '@remotion/player';
import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { Terminal } from '../remotion/compositions';
import { messagingArcScript } from '../data/terminals/messaging-arc';
import { scriptDurationFrames } from '../lib/terminalDuration';

const FPS = 30;
const SPEED_FACTOR = 0.3;
const DURATION = Math.ceil(scriptDurationFrames(messagingArcScript, FPS, 60) / SPEED_FACTOR);

/**
 * Slide 53 — Terminal: Full Messaging Arc.
 */
export function Slide53Terminal(_props: SlideProps) {
  return (
    <SlideFrame act="PART C · EPIC 9 DEEP DIVE" kicker="LIVE DEMO">
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
        <span className="text-gradient">The Full Arc</span>
      </h1>

      <div
        data-record-dwell-ms={Math.ceil((DURATION / FPS) * 1000)}
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
            script: messagingArcScript,
            promptLabel: 'incollege $ ',
            title: 'messaging — full arc',
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

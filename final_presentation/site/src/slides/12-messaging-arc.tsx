import { Player } from '@remotion/player';
import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { Terminal } from '../remotion/compositions';
import { messagingArcScript } from '../data/terminals/messaging-arc';
import { scriptDurationFrames } from '../lib/terminalDuration';

const FPS = 30;
const SPEED_FACTOR = 0.5;
const DURATION_IN_FRAMES = Math.ceil(
  scriptDurationFrames(messagingArcScript, FPS, 90) / SPEED_FACTOR,
);

/**
 * Slide 12 — Melaine — "Messaging, end to end".
 *
 * Epic #9's demo moment. One oversized terminal walks the full arc:
 * Alice sends → clear → Bob logs in → views. Below the terminal, a
 * brand-blue rule and a muted callout hammer home the recipient
 * isolation payoff, with a second dim note citing the COBOL-level
 * guarantees.
 */
export function Slide12MessagingArc(_props: SlideProps) {
  return (
    <SlideFrame act="ACT II · PRODUCT DEMO" kicker="EPIC #9 · MESSAGING">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 88,
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
          lineHeight: 1.02,
          textAlign: 'center',
        }}
      >
        Messaging, <span className="text-gradient">end to end.</span>
      </h1>

      <div
        style={{
          flex: 1,
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'center',
          gap: 28,
          minHeight: 0,
        }}
      >
        <div
          style={{
            width: '75%',
            flex: 1,
            minHeight: 0,
            borderRadius: 22,
            overflow: 'hidden',
            boxShadow:
              '0 70px 140px -40px rgba(0,0,0,0.85), 0 0 0 1px rgba(112,181,249,0.14)',
          }}
        >
          <Player
            component={Terminal}
            inputProps={{
              script: messagingArcScript,
              promptLabel: 'incollege $ ',
              title: 'alice → bob — messages',
              accent: 'brand',
              speedFactor: SPEED_FACTOR,
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
            width: '75%',
            display: 'flex',
            flexDirection: 'column',
            alignItems: 'center',
            gap: 14,
          }}
        >
          <div
            aria-hidden
            style={{
              width: '100%',
              height: 2,
              borderRadius: 2,
              background:
                'linear-gradient(90deg, rgba(10,102,194,0) 0%, rgba(10,102,194,0.9) 50%, rgba(10,102,194,0) 100%)',
              boxShadow: '0 0 24px rgba(56,189,248,0.35)',
            }}
          />
          <p
            style={{
              margin: 0,
              fontFamily: 'var(--font-body)',
              fontStyle: 'italic',
              fontSize: 22,
              color: 'var(--color-text-muted)',
              textAlign: 'center',
              lineHeight: 1.35,
            }}
          >
            Alice's inbox stays empty — recipient isolation in action.
          </p>
          <p
            style={{
              margin: 0,
              fontFamily: 'var(--font-body)',
              fontSize: 13,
              color: 'var(--color-text-dim)',
              textAlign: 'center',
              letterSpacing: '0.04em',
            }}
          >
            Connection-gated send · chronological view · recipient filter at
            the COBOL level · Epic #9's payoff
          </p>
        </div>
      </div>
    </SlideFrame>
  );
}

import { Player } from '@remotion/player';
import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { Terminal } from '../remotion/compositions';
import { connectAcceptScript } from '../data/terminals/connect-accept';
import { scriptDurationFrames } from '../lib/terminalDuration';

const FPS = 30;
const DURATION_IN_FRAMES = scriptDurationFrames(connectAcceptScript, FPS, 60);

const STEPS = ['Send Request', 'Accept', 'View Network'] as const;

/**
 * Slide 08 — Olga — "Connect → Accept → Network".
 *
 * The whole slide is the terminal. One big player walks the three-step
 * arc across two user sessions; a row of brand-accent pill chips below
 * narrates the flow.
 */
export function Slide08ConnectAcceptNetwork(_props: SlideProps) {
  return (
    <SlideFrame act="ACT II · PRODUCT DEMO" kicker="EPICS #4–5 · CONNECTIONS">
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
        Connect <span className="text-gradient">→</span> Accept{' '}
        <span className="text-gradient">→</span> Network.
      </h1>

      <div
        style={{
          flex: 1,
          display: 'flex',
          flexDirection: 'column',
          gap: 32,
          minHeight: 0,
          alignItems: 'center',
        }}
      >
        <div
          style={{
            width: '100%',
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
              script: connectAcceptScript,
              promptLabel: 'incollege $ ',
              title: 'alice ↔ bob — connections',
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
            alignItems: 'center',
            justifyContent: 'center',
            gap: 18,
            flexWrap: 'wrap',
          }}
        >
          {STEPS.map((step, i) => (
            <div key={step} style={{ display: 'flex', alignItems: 'center', gap: 18 }}>
              <div
                style={{
                  padding: '12px 26px',
                  borderRadius: 999,
                  background: 'rgba(10,102,194,0.22)',
                  border: '1px solid rgba(112,181,249,0.42)',
                  color: 'var(--color-brand-accent)',
                  fontFamily: 'var(--font-display)',
                  fontWeight: 700,
                  fontSize: 18,
                  letterSpacing: '0.08em',
                  textTransform: 'uppercase',
                  boxShadow:
                    '0 0 24px rgba(56,189,248,0.18), inset 0 1px 0 rgba(255,255,255,0.06)',
                }}
              >
                {step}
              </div>
              {i < STEPS.length - 1 && (
                <div
                  style={{
                    width: 36,
                    height: 2,
                    background:
                      'linear-gradient(90deg, rgba(112,181,249,0) 0%, rgba(112,181,249,0.7) 50%, rgba(112,181,249,0) 100%)',
                  }}
                />
              )}
            </div>
          ))}
        </div>
      </div>
    </SlideFrame>
  );
}

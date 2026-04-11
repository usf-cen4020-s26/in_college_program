import { Player } from '@remotion/player';
import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { FeatureCallout } from '../components/FeatureCallout';
import { Terminal } from '../remotion/compositions';
import { createAccountScript } from '../data/terminals/create-account';
import { scriptDurationFrames } from '../lib/terminalDuration';

const FPS = 30;
const SPEED_FACTOR = 0.5;
const DURATION = scriptDurationFrames(createAccountScript, FPS, 60) / SPEED_FACTOR;

/**
 * Slide 09 — Terminal: Create Account.
 * Left: terminal player. Right: 3 FeatureCallouts.
 */
export function Slide09M1Terminal(_props: SlideProps) {
  return (
    <SlideFrame act="PART B · BUILDING INCOLLEGE" kicker="EPIC #1 · LIVE DEMO">
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
        Create an <span className="text-gradient">account.</span>
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
              script: createAccountScript,
              promptLabel: 'incollege $ ',
              title: 'alice@incollege — ~',
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
            title="Password policy"
            detail="8–12 chars · 1 upper · 1 digit · 1 special. Anything weaker bounces and the prompt loops."
            citation="src/AUTH.cpy:4400-VALIDATE-PASSWORD"
          />
          <FeatureCallout
            title="Account cap"
            detail="Max 5 accounts in this MVP — WS-CONST-MAX-ACCOUNTS is the single source of truth."
            citation="src/WS-CONSTANTS.cpy"
          />
          <FeatureCallout
            title="Persisted on save"
            detail="Every accepted account is flushed to ACCOUNTS.DAT so restarts keep every credential."
            citation="src/AUTH.cpy:4000-CREATE-ACCOUNT"
          />
        </div>
      </div>
    </SlideFrame>
  );
}

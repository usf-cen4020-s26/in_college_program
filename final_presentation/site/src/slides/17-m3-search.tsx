import { Player } from '@remotion/player';
import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { FeatureCallout } from '../components/FeatureCallout';
import { StatCard } from '../components/StatCard';
import { Terminal } from '../remotion/compositions';
import { findSomeoneScript } from '../data/terminals/find-someone';
import { scriptDurationFrames } from '../lib/terminalDuration';

const FPS = 30;
const SPEED_FACTOR = 0.5;
const DURATION = scriptDurationFrames(findSomeoneScript, FPS, 60) / SPEED_FACTOR;

/**
 * Slide 17 — Find Someone You Know.
 * Left: terminal. Right: callout + stat.
 */
export function Slide17M3Search(_props: SlideProps) {
  return (
    <SlideFrame act="PART B · BUILDING INCOLLEGE" kicker="EPIC #3 · SEARCH">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(36px, 4em, 64px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
          lineHeight: 1.05,
        }}
      >
        Find Someone You Know
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
          data-record-dwell-ms={Math.ceil((DURATION / FPS) * 1000)}
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
              script: findSomeoneScript,
              promptLabel: 'incollege $ ',
              title: 'alice@incollege — search',
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
            gap: 24,
            justifyContent: 'center',
          }}
        >
          <FeatureCallout
            title="Search by name"
            detail="First + last name lookup scans ACCOUNTS.DAT and returns whether the user is in the system."
            citation="src/SEARCH.cpy"
          />
          <StatCard value="~58" label="tests running total" />
        </div>
      </div>
    </SlideFrame>
  );
}

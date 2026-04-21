import { Player } from '@remotion/player';
import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { FeatureCallout } from '../components/FeatureCallout';
import { Terminal } from '../remotion/compositions';
import { browseApplyScript } from '../data/terminals/browse-apply';
import { scriptDurationFrames } from '../lib/terminalDuration';

const FPS = 30;
const SPEED_FACTOR = 0.5;
const DURATION_IN_FRAMES = Math.ceil(
  scriptDurationFrames(browseApplyScript, FPS, 60) / SPEED_FACTOR,
);

/**
 * Slide 10 — Victoria — "Browse. Read. Apply".
 *
 * Left: terminal walking the list → detail → apply → confirmation
 * arc end-to-end.
 * Right: three callouts: the three-keystroke flow, the duplicate guard,
 * and the confirmation echo.
 */
export function Slide10BrowseApply(_props: SlideProps) {
  return (
    <SlideFrame act="ACT II · PRODUCT DEMO" kicker="EPIC #7 · BROWSE & APPLY">
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
        Browse. Read. <span className="text-gradient">Apply.</span>
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
              script: browseApplyScript,
              promptLabel: 'incollege $ ',
              title: 'alice@incollege — browse jobs',
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
            display: 'flex',
            flexDirection: 'column',
            gap: 20,
            justifyContent: 'center',
          }}
        >
          <FeatureCallout
            title="Three keystrokes"
            detail="List view → detail view → apply. No extra screens, no confirmation modals — the fastest path from discovery to application is kept deliberately short."
            citation="src/BROWSEJOBS.cpy · APPLYJOB.cpy"
          />
          <FeatureCallout
            title="Duplicate-application guard"
            detail="Before writing, the apply path scans APPLICATIONS.DAT for a matching (user, job) pair and short-circuits with a clean already-applied message."
            citation="src/APPLYJOB.cpy:5327"
          />
          <FeatureCallout
            title="Confirmation echoes the posting"
            detail="The success line names the job title and employer back at the user, so they always know exactly which posting they just applied to."
            citation="src/APPLYJOB.cpy"
          />
        </div>
      </div>
    </SlideFrame>
  );
}

import { Player } from '@remotion/player';
import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { FeatureCallout } from '../components/FeatureCallout';
import { Terminal } from '../remotion/compositions';
import { findSomeoneScript } from '../data/terminals/find-someone';
import { scriptDurationFrames } from '../lib/terminalDuration';

const FPS = 30;
const SPEED_FACTOR = 0.5;
const DURATION_IN_FRAMES = Math.ceil(
  scriptDurationFrames(findSomeoneScript, FPS, 60) / SPEED_FACTOR,
);

/**
 * Slide 07 — Olga — "Find someone you know".
 *
 * Left: terminal playback of a deterministic exact-match search that
 * surfaces the matching profile inline.
 * Right: two callouts + a footnote on the MVP's privacy-first decision
 * to keep search behind login.
 */
export function Slide07FindSomeone(_props: SlideProps) {
  return (
    <SlideFrame act="ACT II · PRODUCT DEMO" kicker="EPIC #3 · DISCOVERY">
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
        Find someone <span className="text-gradient">you know.</span>
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
              script: findSomeoneScript,
              promptLabel: 'incollege $ ',
              title: 'alice@incollege — search',
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
            title="Exact full-name match"
            detail="Deterministic equality against first + last — no fuzzy matching, no ranked results, no magic. You get a hit or you get a clean miss."
            citation="src/SEARCH.cpy:7500-FIND-SOMEONE-YOU-KNOW"
          />
          <FeatureCallout
            title="Profile surfaces inline"
            detail="When the match lands, the other student's full profile opens right in the same session — same layout as View My Profile."
            citation="src/PROFILE.cpy · view path"
          />
          <p
            style={{
              margin: 0,
              marginTop: 8,
              fontFamily: 'var(--font-body)',
              fontSize: 13,
              fontStyle: 'italic',
              color: 'var(--color-text-dim)',
              lineHeight: 1.5,
            }}
          >
            Note — search lives behind login in this MVP, a privacy-first
            decision for student data.
          </p>
        </div>
      </div>
    </SlideFrame>
  );
}

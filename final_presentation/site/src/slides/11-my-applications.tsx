import { Player } from '@remotion/player';
import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { FeatureCallout } from '../components/FeatureCallout';
import { Terminal } from '../remotion/compositions';
import { myApplicationsScript } from '../data/terminals/my-applications';
import { scriptDurationFrames } from '../lib/terminalDuration';

const FPS = 30;
const SPEED_FACTOR = 0.5;
const DURATION_IN_FRAMES = Math.ceil(
  scriptDurationFrames(myApplicationsScript, FPS, 60) / SPEED_FACTOR,
);

/**
 * Slide 11 — Victoria — "See where you applied".
 *
 * Left: terminal replay of View My Applications showing the filtered
 * per-user report.
 * Right: two callouts — the per-user filter and the formatted report.
 */
export function Slide11MyApplications(_props: SlideProps) {
  return (
    <SlideFrame act="ACT II · PRODUCT DEMO" kicker="EPIC #7 · REPORTS">
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
        See <span className="text-gradient">where you applied.</span>
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
              script: myApplicationsScript,
              promptLabel: 'incollege $ ',
              title: 'alice@incollege — my applications',
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
            title="Per-user filter"
            detail="The report walks APPLICATIONS.DAT and only surfaces records whose applicant matches the logged-in user — other students' applications never leak into the view."
            citation="src/VIEWAPPS.cpy"
          />
          <FeatureCallout
            title="Formatted report"
            detail="Each row prints title, employer, and location in a consistent layout, and the footer echoes the total count so the student always knows where they stand."
            citation="src/VIEWAPPS.cpy · display paragraphs"
          />
        </div>
      </div>
    </SlideFrame>
  );
}

import { Player } from '@remotion/player';
import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { FeatureCallout } from '../components/FeatureCallout';
import { StepReveal } from '../components/StepReveal';
import { Terminal } from '../remotion/compositions';
import { browseApplyScript } from '../data/terminals/browse-apply';
import { scriptDurationFrames } from '../lib/terminalDuration';

const FPS = 30;
const SPEED_FACTOR = 0.5;
const DURATION = scriptDurationFrames(browseApplyScript, FPS, 60) / SPEED_FACTOR;

/**
 * Slide 39 — Browse, Apply, Report.
 */
export function Slide39M7Browse({ step }: SlideProps) {
  return (
    <SlideFrame act="PART B · BUILDING INCOLLEGE" kicker="EPIC #7 · APPLICATIONS">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(44px, 5em, 80px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
          lineHeight: 1.02,
        }}
      >
        Browse, Apply, <span className="text-gradient">Report</span>
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
              title: 'alice@incollege — browse',
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
            title="Browse Summary View"
            detail="See all available jobs at a glance: title, employer, and location."
            citation="src/BROWSEJOBS.cpy:5200"
          />
          <FeatureCallout
            title="View Details"
            detail="Drill into any listing for the full description and salary."
            citation="src/BROWSEJOBS.cpy:5250"
          />
          <FeatureCallout
            title="Apply"
            detail="One-click apply writes to APPLICATIONS.DAT with the poster recorded."
            citation="src/APPLYJOB.cpy:5300"
          />
          <StepReveal currentStep={step} visibleAt={1}>
            <div style={{ display: 'flex', flexDirection: 'column', gap: 20 }}>
              <FeatureCallout
                title="Duplicate Prevention"
                detail="Cannot apply to the same job twice — checked before writing."
                citation="src/APPLYJOB.cpy:5327"
              />
              <FeatureCallout
                title="Application Summary Report"
                detail="View My Applications shows every job you applied to with status."
                citation="src/VIEWAPPS.cpy:5400"
              />
            </div>
          </StepReveal>
        </div>
      </div>
    </SlideFrame>
  );
}

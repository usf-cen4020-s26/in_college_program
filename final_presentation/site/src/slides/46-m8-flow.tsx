import { Player } from '@remotion/player';
import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';
import { Terminal } from '../remotion/compositions';
import { messagingArcScript } from '../data/terminals/messaging-arc';
import { scriptDurationFrames } from '../lib/terminalDuration';

const FPS = 30;
const SPEED_FACTOR = 0.5;
const DURATION = scriptDurationFrames(messagingArcScript, FPS, 60) / SPEED_FACTOR;

const ARROW_STYLE: React.CSSProperties = {
  textAlign: 'center' as const,
  fontSize: 20,
  color: 'var(--color-brand-accent)',
  lineHeight: 1,
  padding: '4px 0',
};

const BOX_STYLE: React.CSSProperties = {
  padding: '14px 20px',
  borderRadius: 14,
  background: 'var(--color-bg-panel)',
  border: '1px solid rgba(112,181,249,0.14)',
  fontFamily: 'var(--font-body)',
  fontSize: 'clamp(14px, 1.1em, 18px)',
  color: 'var(--color-text-primary)',
  textAlign: 'center' as const,
  boxShadow: '0 8px 24px -12px rgba(0,0,0,0.6)',
};

/**
 * Slide 46 — The Message Flow.
 * Left: terminal player. Right: step-based flow diagram.
 */
export function Slide46M8Flow({ step }: SlideProps) {
  return (
    <SlideFrame act="PART B · BUILDING INCOLLEGE" kicker="EPIC #8 · MESSAGING">
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
        <span className="text-gradient">The Message Flow</span>
      </h1>

      <div
        style={{
          display: 'grid',
          gridTemplateColumns: '1.6fr 1fr',
          gap: 48,
          flex: 1,
          minHeight: 0,
          alignItems: 'stretch',
        }}
      >
        {/* Terminal */}
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
              script: messagingArcScript,
              promptLabel: 'incollege $ ',
              title: 'messaging session',
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

        {/* Flow diagram */}
        <div
          style={{
            display: 'flex',
            flexDirection: 'column',
            justifyContent: 'center',
            gap: 0,
          }}
        >
          <StepReveal currentStep={step} visibleAt={0}>
            <div style={{ display: 'flex', flexDirection: 'column', gap: 0 }}>
              <div style={BOX_STYLE}>Enter recipient</div>
              <div style={ARROW_STYLE}>&#8595;</div>
              <div style={BOX_STYLE}>Validate exists</div>
              <div style={ARROW_STYLE}>&#8595;</div>
              <div style={BOX_STYLE}>Validate connection</div>
            </div>
          </StepReveal>

          <StepReveal currentStep={step} visibleAt={1}>
            <div style={{ display: 'flex', flexDirection: 'column', gap: 0, marginTop: 8 }}>
              <div style={ARROW_STYLE}>&#8595;</div>
              <div style={BOX_STYLE}>Enter message</div>
              <div style={ARROW_STYLE}>&#8595;</div>
              <div style={BOX_STYLE}>Auto-timestamp</div>
              <div style={ARROW_STYLE}>&#8595;</div>
              <div style={BOX_STYLE}>Write to MESSAGES.DAT</div>
            </div>
          </StepReveal>
        </div>
      </div>
    </SlideFrame>
  );
}

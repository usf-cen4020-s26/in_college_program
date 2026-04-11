import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { Player } from '@remotion/player';
import { StatCounter } from '../remotion/compositions';

const FPS = 30;
const DURATION = 120;

const STATS = [
  { value: 9, label: 'Epics', suffix: '' },
  { value: 28, label: 'Source Files', suffix: '' },
  { value: 4350, label: 'Lines of COBOL', suffix: '+' },
  { value: 195, label: 'Test Fixtures', suffix: '' },
  { value: 7, label: '.DAT Files', suffix: '' },
] as const;

/**
 * Slide 6 — By the Numbers. Five animated stat counters.
 */
export function Slide06Numbers(_props: SlideProps) {
  return (
    <SlideFrame act="PART A · AGILE AT A GLANCE" variant="hero" showWordmark>
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(36px, 4.5em, 72px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
          textAlign: 'center',
        }}
      >
        By the <span className="text-gradient">Numbers.</span>
      </h1>

      <div
        style={{
          flex: 1,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
        }}
      >
        <div
          style={{
            display: 'flex',
            gap: 'clamp(12px, 2em, 40px)',
            flexWrap: 'wrap',
            justifyContent: 'center',
          }}
        >
          {STATS.map((stat, i) => (
            <div
              key={stat.label}
              style={{
                display: 'flex',
                flexDirection: 'column',
                alignItems: 'center',
                gap: 8,
                padding: 'clamp(16px, 1.5em, 28px)',
                borderRadius: 16,
                background: 'var(--color-bg-card)',
                border: '1px solid rgba(133,197,255,0.1)',
                minWidth: 'clamp(120px, 10em, 180px)',
                animation: `s01-rise 800ms cubic-bezier(0.16,1,0.3,1) both ${200 + i * 100}ms`,
              }}
            >
              <div
                style={{
                  width: 'clamp(100px, 8em, 160px)',
                  height: 'clamp(48px, 4em, 72px)',
                }}
              >
                <Player
                  component={StatCounter}
                  inputProps={{
                    to: stat.value,
                    suffix: stat.suffix,
                  }}
                  durationInFrames={DURATION}
                  fps={FPS}
                  compositionWidth={400}
                  compositionHeight={160}
                  autoPlay
                  loop={false}
                  controls={false}
                  clickToPlay={false}
                  style={{ width: '100%', height: '100%' }}
                />
              </div>
              <span
                style={{
                  fontFamily: 'var(--font-body)',
                  fontSize: 'clamp(10px, 0.8em, 14px)',
                  fontWeight: 600,
                  letterSpacing: '0.12em',
                  textTransform: 'uppercase',
                  color: 'var(--color-text-dim)',
                }}
              >
                {stat.label}
              </span>
            </div>
          ))}
        </div>
      </div>

      <style>{`
        @keyframes s01-rise {
          0% { opacity: 0; transform: translateY(14px); }
          100% { opacity: 1; transform: translateY(0); }
        }
      `}</style>
    </SlideFrame>
  );
}

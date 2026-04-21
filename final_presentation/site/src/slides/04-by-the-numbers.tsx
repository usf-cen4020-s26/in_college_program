import type { SlideProps } from './types';
import { Player } from '@remotion/player';
import { SlideFrame } from '../components/SlideFrame';
import { StatCounter } from '../remotion/compositions';
import { STATS } from '../data/stats';

/**
 * Counter-up stat strip. Five Remotion Players mount the StatCounter
 * composition, each counting from 0 to its target on entry. Every card
 * sits in a matching bg-panel frame so the row reads as a cohesive set.
 */
type StatEntry = {
  to: number;
  suffix?: string;
  displayOverride?: string;
  label: string;
  sublabel: string;
};

const STAT_ENTRIES: readonly StatEntry[] = [
  {
    to: STATS.epics,
    label: 'EPICS',
    sublabel: 'completed this semester',
  },
  {
    to: STATS.modules,
    label: 'MODULES',
    sublabel: 'main.cob + 24 copybooks',
  },
  {
    to: STATS.loc,
    suffix: '+',
    displayOverride: '2.2K LOC',
    label: 'LINES',
    sublabel: 'of production COBOL',
  },
  {
    to: STATS.tests,
    label: 'TESTS',
    sublabel: 'across 9 categories',
  },
  {
    to: STATS.dataFiles,
    label: 'DATA FILES',
    sublabel: 'persistent, append-only',
  },
];

export function Slide04ByTheNumbers(_props: SlideProps) {
  return (
    <SlideFrame act="ACT I · THE PITCH" kicker="BY THE NUMBERS">
      <style>{`
        @keyframes slide04-headline {
          0% { opacity: 0; transform: translateY(20px); }
          100% { opacity: 1; transform: translateY(0); }
        }
        @keyframes slide04-card {
          0% { opacity: 0; transform: translateY(28px); }
          100% { opacity: 1; transform: translateY(0); }
        }
        .slide04-headline {
          animation: slide04-headline 900ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 80ms;
        }
        .slide04-card {
          animation: slide04-card 900ms cubic-bezier(0.16, 1, 0.3, 1) both;
        }
      `}</style>

      <h1
        className="slide04-headline"
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontWeight: 700,
          fontSize: 84,
          letterSpacing: '-0.025em',
          lineHeight: 1.02,
          color: 'var(--color-text-primary)',
        }}
      >
        What we <span className="text-gradient">shipped.</span>
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
            gap: 28,
            width: '100%',
            justifyContent: 'space-between',
            maxWidth: 1700,
          }}
        >
          {STAT_ENTRIES.map((stat, i) => (
            <div
              key={stat.label}
              className="slide04-card"
              style={{
                flex: '1 1 0',
                maxWidth: 320,
                minWidth: 0,
                animationDelay: `${280 + i * 140}ms`,
                background: 'var(--color-bg-panel)',
                border: '1px solid rgba(112,181,249,0.14)',
                borderRadius: 20,
                padding: '24px 18px 26px',
                display: 'flex',
                flexDirection: 'column',
                alignItems: 'center',
                gap: 14,
                boxShadow:
                  '0 30px 60px -32px rgba(0,0,0,0.8), inset 0 1px 0 rgba(255,255,255,0.04)',
              }}
            >
              <div
                style={{
                  width: '100%',
                  aspectRatio: '16 / 9',
                  overflow: 'hidden',
                  borderRadius: 14,
                  background:
                    'radial-gradient(ellipse 320px 180px at 50% 30%, rgba(10,102,194,0.22) 0%, rgba(10,102,194,0) 70%)',
                }}
              >
                <Player
                  component={StatCounter}
                  inputProps={{
                    to: stat.to,
                    suffix: stat.suffix,
                    durationInFrames: 40,
                  }}
                  durationInFrames={90}
                  compositionWidth={1920}
                  compositionHeight={1080}
                  fps={30}
                  autoPlay
                  loop
                  controls={false}
                  style={{ width: '100%', height: '100%' }}
                />
              </div>
              <div
                style={{
                  fontFamily: 'var(--font-display)',
                  fontWeight: 700,
                  fontSize: 18,
                  letterSpacing: '0.22em',
                  textTransform: 'uppercase',
                  color: 'var(--color-text-primary)',
                  textAlign: 'center',
                }}
              >
                {stat.label}
              </div>
              <div
                style={{
                  fontFamily: 'var(--font-body)',
                  fontSize: 14,
                  color: 'var(--color-text-muted)',
                  textAlign: 'center',
                  lineHeight: 1.4,
                  minHeight: 36,
                }}
              >
                {stat.sublabel}
                {stat.displayOverride && (
                  <>
                    <br />
                    <span
                      style={{
                        fontFamily: 'var(--font-mono)',
                        fontSize: 12,
                        color: 'var(--color-text-dim)',
                        letterSpacing: '0.02em',
                      }}
                    >
                      ~{stat.displayOverride}
                    </span>
                  </>
                )}
              </div>
            </div>
          ))}
        </div>
      </div>
    </SlideFrame>
  );
}

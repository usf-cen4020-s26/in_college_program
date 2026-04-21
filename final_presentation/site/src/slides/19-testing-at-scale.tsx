import { Player } from '@remotion/player';
import { SlideFrame } from '../components/SlideFrame';
import { StatCounter } from '../remotion/compositions';
import type { StatCounterProps } from '../remotion/compositions';
import { TEST_CATEGORIES } from '../data/stats';
import type { SlideProps } from './types';

const COUNTER_PROPS: StatCounterProps = {
  to: 195,
  durationInFrames: 50,
  label: 'TOTAL FIXTURES',
  sublabel: 'across 9 categories',
};

export function Slide19TestingAtScale(_props: SlideProps) {
  return (
    <SlideFrame act="ACT III · HOW WE BUILT IT" kicker="TESTING">
      <div>
        <h1
          style={{
            margin: 0,
            fontFamily: 'var(--font-display)',
            fontWeight: 700,
            fontSize: 72,
            lineHeight: 1.05,
            letterSpacing: '-0.02em',
            color: 'var(--color-text-primary)',
          }}
        >
          195 fixtures. Zero mocks.
        </h1>
      </div>

      <div
        style={{
          display: 'flex',
          justifyContent: 'center',
          alignItems: 'center',
        }}
      >
        <div
          style={{
            width: 1200,
            height: 340,
            borderRadius: 20,
            overflow: 'hidden',
            background:
              'radial-gradient(ellipse 900px 360px at 50% 50%, rgba(10,102,194,0.18) 0%, rgba(10,102,194,0) 72%)',
          }}
        >
          <Player
            component={StatCounter}
            inputProps={COUNTER_PROPS}
            durationInFrames={120}
            fps={30}
            compositionWidth={1920}
            compositionHeight={1080}
            style={{ width: '100%', height: '100%' }}
            autoPlay
            loop
            controls={false}
          />
        </div>
      </div>

      <div
        style={{
          display: 'grid',
          gridTemplateColumns: 'repeat(3, 1fr)',
          gridTemplateRows: 'repeat(3, 1fr)',
          gap: 18,
          flex: 1,
          minHeight: 0,
        }}
      >
        {TEST_CATEGORIES.map((cat) => (
          <div
            key={cat.category}
            style={{
              padding: '18px 22px',
              borderRadius: 14,
              background: 'var(--color-bg-panel)',
              border: '1px solid rgba(112,181,249,0.14)',
              boxShadow:
                '0 20px 40px -24px rgba(0,0,0,0.75), inset 0 1px 0 rgba(255,255,255,0.04)',
              display: 'flex',
              flexDirection: 'column',
              gap: 6,
              minWidth: 0,
            }}
          >
            <div
              style={{
                display: 'flex',
                alignItems: 'baseline',
                justifyContent: 'space-between',
                gap: 12,
              }}
            >
              <div
                style={{
                  fontFamily: 'var(--font-mono)',
                  fontSize: 15,
                  color: 'var(--color-brand-accent)',
                  letterSpacing: '0.02em',
                  fontWeight: 600,
                  whiteSpace: 'nowrap',
                  overflow: 'hidden',
                  textOverflow: 'ellipsis',
                }}
              >
                {cat.category}
              </div>
              <div
                style={{
                  fontFamily: 'var(--font-mono)',
                  fontSize: 36,
                  fontWeight: 700,
                  color: 'var(--color-text-primary)',
                  lineHeight: 1,
                  fontVariantNumeric: 'tabular-nums',
                }}
              >
                {cat.count}
              </div>
            </div>
            <div
              style={{
                fontFamily: 'var(--font-body)',
                fontSize: 12,
                lineHeight: 1.5,
                color: 'var(--color-text-muted)',
              }}
            >
              {cat.description}
            </div>
          </div>
        ))}
      </div>

      <div
        style={{
          textAlign: 'center',
          fontFamily: 'var(--font-display)',
          fontWeight: 600,
          fontSize: 22,
          color: 'var(--color-text-primary)',
          letterSpacing: '0.01em',
        }}
      >
        “Real COBOL binary. Deterministic diffs. Zero mocks.”
      </div>
    </SlideFrame>
  );
}

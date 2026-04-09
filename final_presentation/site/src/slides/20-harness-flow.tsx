import { Player } from '@remotion/player';
import { SlideFrame } from '../components/SlideFrame';
import { HarnessFlow } from '../remotion/compositions';
import type { HarnessFlowProps, HarnessStep } from '../remotion/compositions';
import type { SlideProps } from './types';

const STEPS: HarnessStep[] = [
  { label: 'Fixture', sublabel: '.in.txt', icon: '\u{1F4C4}' },
  { label: 'Preprocess', sublabel: '@seed_* macros', icon: '\u2699' },
  { label: 'Seed + Write', sublabel: 'INPUT.TXT · .DAT', icon: '\u{1F4E5}' },
  { label: 'Run Binary', sublabel: 'bin/main', icon: '\u{1F680}' },
  { label: 'Normalize', sublabel: 'expand · stamps', icon: '\u{1F504}' },
  { label: 'Diff', sublabel: 'difflib', icon: '' },
];

const HARNESS_PROPS: HarnessFlowProps = { steps: STEPS };

const FACTOIDS: readonly string[] = [
  'Timestamp normalization keeps `Sent: YYYY-MM-DD ...` stable across runs.',
  'Multi-part tests validate persistence — write in run 1, read in run 2.',
  '70 of 195 fixtures are multi-part.',
];

export function Slide20HarnessFlow(_props: SlideProps) {
  return (
    <SlideFrame act="ACT III · HOW WE BUILT IT" kicker="HOW WE KNOW">
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
          Every fixture runs the real binary.
        </h1>
      </div>

      <div
        style={{
          flex: 1,
          minHeight: 0,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
        }}
      >
        <div
          style={{
            width: '100%',
            maxWidth: 1640,
            aspectRatio: '1920 / 1080',
            maxHeight: 560,
            borderRadius: 22,
            overflow: 'hidden',
            background:
              'radial-gradient(ellipse 1100px 420px at 50% 50%, rgba(10,102,194,0.16) 0%, rgba(10,102,194,0) 72%)',
          }}
        >
          <Player
            component={HarnessFlow}
            inputProps={HARNESS_PROPS}
            durationInFrames={180}
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
          gap: 22,
        }}
      >
        {FACTOIDS.map((text) => (
          <div
            key={text}
            style={{
              padding: '18px 22px',
              borderRadius: 14,
              background: 'var(--color-bg-panel)',
              border: '1px solid rgba(112,181,249,0.12)',
              fontFamily: 'var(--font-body)',
              fontSize: 14,
              lineHeight: 1.55,
              color: 'var(--color-text-muted)',
            }}
          >
            {text}
          </div>
        ))}
      </div>
    </SlideFrame>
  );
}

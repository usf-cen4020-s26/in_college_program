import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { BeforeAfter } from '../components/BeforeAfter';

const BEFORE_CODE = `1
alice
Alice1!
4`;

const AFTER_CODE = `1        # Login
alice    # Username
Alice1!  # Password
4        # Find someone`;

/**
 * Slide 21 — Test Runner Phases 3-4: Comments & Debug Mode.
 * Before/after comparison of test input readability.
 */
export function Slide21TRComments({ step }: SlideProps) {
  return (
    <SlideFrame act="INTERLUDE" kicker="TEST RUNNER · PHASES 3-4">
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
        Comments & Debug Mode
      </h1>

      <BeforeAfter
        step={step}
        beforeLabel="Before"
        afterLabel="After"
        before={
          <pre
            style={{
              margin: 0,
              fontFamily: 'var(--font-mono)',
              fontSize: 'clamp(14px, 1.2em, 20px)',
              lineHeight: 1.8,
              color: 'var(--color-text-primary)',
              whiteSpace: 'pre',
            }}
          >
            {BEFORE_CODE}
          </pre>
        }
        after={
          <pre
            style={{
              margin: 0,
              fontFamily: 'var(--font-mono)',
              fontSize: 'clamp(14px, 1.2em, 20px)',
              lineHeight: 1.8,
              color: 'var(--color-text-primary)',
              whiteSpace: 'pre',
            }}
          >
            {AFTER_CODE}
          </pre>
        }
      />
    </SlideFrame>
  );
}

import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { FeatureCallout } from '../components/FeatureCallout';

/**
 * Slide 33 — Why Seeds Matter: 3 feature callouts.
 */
export function Slide33WhySeeds(_props: SlideProps) {
  return (
    <SlideFrame act="INTERLUDE" kicker="SEED DIRECTIVES · WHY">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(40px, 5em, 80px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
          lineHeight: 1.05,
        }}
      >
        Why This <span className="text-gradient">Matters</span>
      </h1>

      <div
        style={{
          display: 'grid',
          gridTemplateColumns: 'repeat(3, 1fr)',
          gap: 'clamp(16px, 1.5em, 28px)',
          flex: 1,
          alignItems: 'start',
        }}
      >
        <FeatureCallout
          title="Faster"
          detail="No COBOL execution for setup — data written directly to .DAT files"
        />
        <FeatureCallout
          title="Isolated"
          detail="Each test starts from a clean, known state"
        />
        <FeatureCallout
          title="Resilient"
          detail="Immune to UI changes in unrelated features"
        />
      </div>
    </SlideFrame>
  );
}

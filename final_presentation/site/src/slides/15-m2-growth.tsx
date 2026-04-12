import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StatCard } from '../components/StatCard';

/**
 * Slide 15 — Growing Pains.
 * Three stat cards + a note about monolithic code.
 */
export function Slide15M2Growth(_props: SlideProps) {
  return (
    <SlideFrame act="PART B · BUILDING INCOLLEGE" kicker="WEEK 3 · GROWING">
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
        Growing Pains
      </h1>

      <div
        style={{
          display: 'grid',
          gridTemplateColumns: 'repeat(3, 1fr)',
          gap: 32,
          alignItems: 'start',
          flex: 1,
          alignContent: 'center',
        }}
      >
        <StatCard value="1,220" label="lines in main.cob" />
        <StatCard value="2" label="DAT files" />
        <StatCard value="~28" label="test fixtures" />
      </div>

      <p
        style={{
          margin: 0,
          fontFamily: 'var(--font-body)',
          fontSize: 'clamp(16px, 1.4em, 24px)',
          lineHeight: 1.5,
          color: 'var(--color-text-muted)',
          textAlign: 'center',
        }}
      >
        Still monolithic — everything lives in one file. <em>Remember that number:
        fixtures are about to explode.</em>
      </p>
      <p
        style={{
          margin: 0,
          fontFamily: 'var(--font-mono)',
          fontSize: 'clamp(10px, 0.72em, 13px)',
          letterSpacing: '0.14em',
          textTransform: 'uppercase',
          color: 'var(--color-brand-accent)',
          textAlign: 'center',
          opacity: 0.72,
        }}
      >
        → We revisit this in Interlude B: Macros &amp; Seeds
      </p>
    </SlideFrame>
  );
}

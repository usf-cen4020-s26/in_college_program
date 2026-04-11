import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StatCard } from '../components/StatCard';

/**
 * Slide 40 — Running Totals at Week 9.
 */
export function Slide40M7Progress(_props: SlideProps) {
  return (
    <SlideFrame act="PART B · BUILDING INCOLLEGE" kicker="PROGRESS · WEEK 9">
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
        Running <span className="text-gradient">Totals</span>
      </h1>

      <div
        style={{
          display: 'grid',
          gridTemplateColumns: 'repeat(3, 1fr)',
          gap: 'clamp(20px, 2em, 40px)',
          flex: 1,
          alignItems: 'center',
        }}
      >
        <StatCard value="150+" label="Tests" sublabel="automated regression tests" />
        <StatCard value="6" label="DAT Files" sublabel="persistent data stores" tone="success" />
        <StatCard value="~1,950" label="Lines in main.cob" sublabel="one monolithic file" tone="warn" />
      </div>

      <p
        style={{
          margin: 0,
          fontFamily: 'var(--font-body)',
          fontSize: 'clamp(16px, 1.25em, 22px)',
          color: 'var(--color-text-muted)',
          textAlign: 'center',
          fontStyle: 'italic',
        }}
      >
        main.cob is approaching 2,000 lines. Something has to give...
      </p>
    </SlideFrame>
  );
}

import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StatCard } from '../components/StatCard';

const STATS = [
  { value: '9', label: 'epics completed' },
  { value: '28', label: 'source files' },
  { value: '4,350+', label: 'lines of COBOL' },
  { value: '195', label: 'test fixtures' },
  { value: '7', label: '.DAT data files' },
  { value: '~65', label: 'git commits' },
  { value: '15+', label: 'PRs merged' },
  { value: '11 weeks', label: 'Jan 21 – Apr 8' },
];

/**
 * Slide 60 — Final Statistics.
 */
export function Slide60FinalStats(_props: SlideProps) {
  return (
    <SlideFrame act="PART D · ARCHITECTURE" kicker="BY THE NUMBERS" variant="hero">
      <div
        style={{
          flex: 1,
          display: 'grid',
          gridTemplateColumns: 'repeat(4, 1fr)',
          gridTemplateRows: '1fr 1fr',
          gap: 'clamp(12px, 1.5em, 24px)',
          alignContent: 'center',
          minHeight: 0,
          overflow: 'hidden',
        }}
      >
        {STATS.map((s) => (
          <StatCard key={s.label} value={s.value} label={s.label} />
        ))}
      </div>
    </SlideFrame>
  );
}

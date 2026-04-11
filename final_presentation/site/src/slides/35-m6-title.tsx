import type { SlideProps } from './types';
import { SectionTitle } from '../components/SectionTitle';

/**
 * Slide 35 — Milestone 6: Job Posting title card.
 */
export function Slide35M6Title(_props: SlideProps) {
  return (
    <SectionTitle
      kicker="MILESTONE 6"
      title="Job Posting"
      dateRange="Mar 12"
      epicNumber={6}
      act="PART B · BUILDING INCOLLEGE"
    />
  );
}

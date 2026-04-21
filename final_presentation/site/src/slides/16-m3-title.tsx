import type { SlideProps } from './types';
import { SectionTitle } from '../components/SectionTitle';

/**
 * Slide 16 — Milestone 3: User Search title card.
 */
export function Slide16M3Title(_props: SlideProps) {
  return (
    <SectionTitle
      kicker="MILESTONE 3"
      title="User Search"
      dateRange="Feb 12"
      epicNumber={3}
      act="PART B · BUILDING INCOLLEGE"
    />
  );
}

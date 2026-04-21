import type { SlideProps } from './types';
import { SectionTitle } from '../components/SectionTitle';

/**
 * Slide 23 — Milestone 4: Connection Requests title card.
 */
export function Slide23M4Title(_props: SlideProps) {
  return (
    <SectionTitle
      kicker="MILESTONE 4"
      title="Connections"
      dateRange="Feb 19"
      epicNumber={4}
      act="PART B · BUILDING INCOLLEGE"
    />
  );
}

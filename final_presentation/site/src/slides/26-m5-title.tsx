import type { SlideProps } from './types';
import { SectionTitle } from '../components/SectionTitle';

/**
 * Slide 26 — Milestone 5: Accept/Reject & Network title card.
 */
export function Slide26M5Title(_props: SlideProps) {
  return (
    <SectionTitle
      kicker="MILESTONE 5"
      title="Network"
      dateRange="Mar 5"
      epicNumber={5}
      act="PART B · BUILDING INCOLLEGE"
    />
  );
}

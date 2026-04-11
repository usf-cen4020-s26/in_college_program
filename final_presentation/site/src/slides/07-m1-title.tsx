import type { SlideProps } from './types';
import { SectionTitle } from '../components/SectionTitle';

/**
 * Slide 07 — Milestone 1: Authentication title card.
 */
export function Slide07M1Title(_props: SlideProps) {
  return (
    <SectionTitle
      kicker="MILESTONE 1"
      title="Authentication"
      dateRange="Jan 21 – Jan 27"
      epicNumber={1}
      act="PART B · BUILDING INCOLLEGE"
    />
  );
}

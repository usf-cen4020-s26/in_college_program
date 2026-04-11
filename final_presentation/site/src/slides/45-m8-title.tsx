import type { SlideProps } from './types';
import { SectionTitle } from '../components/SectionTitle';

/**
 * Slide 45 — Milestone 8: Send Messages title card.
 */
export function Slide45M8Title(_props: SlideProps) {
  return (
    <SectionTitle
      kicker="MILESTONE 8"
      title="Send Messages"
      dateRange="Apr 2"
      epicNumber={8}
      act="PART B · BUILDING INCOLLEGE"
    />
  );
}

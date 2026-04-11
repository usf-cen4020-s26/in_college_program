import type { SlideProps } from './types';
import { SectionTitle } from '../components/SectionTitle';

/**
 * Slide 13 — Milestone 2: User Profiles title card.
 */
export function Slide13M2Title(_props: SlideProps) {
  return (
    <SectionTitle
      kicker="MILESTONE 2"
      title="User Profiles"
      dateRange="Feb 1"
      epicNumber={2}
      act="PART B · BUILDING INCOLLEGE"
    />
  );
}

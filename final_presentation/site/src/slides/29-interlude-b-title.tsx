import type { SlideProps } from './types';
import { QuoteBreakout } from '../components/QuoteBreakout';

/**
 * Slide 29 — Interlude B opener: Macros & Seeds.
 */
export function Slide29IntBTitle(_props: SlideProps) {
  return (
    <QuoteBreakout
      kicker="INTERLUDE B"
      quote="Every time the menu changed, we had to update 100+ test files. There had to be a better way."
    />
  );
}

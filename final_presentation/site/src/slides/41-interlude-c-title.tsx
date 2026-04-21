import type { SlideProps } from './types';
import { QuoteBreakout } from '../components/QuoteBreakout';

/**
 * Slide 41 — Interlude C opener: The Great Modularization.
 */
export function Slide41IntCTitle(_props: SlideProps) {
  return (
    <QuoteBreakout
      kicker="INTERLUDE C"
      quote="main.cob was approaching 2,000 lines. It was time to break it apart."
    />
  );
}

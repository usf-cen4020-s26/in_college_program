import type { SlideProps } from './types';
import { QuoteBreakout } from '../components/QuoteBreakout';

/**
 * Slide 18 — Interlude A: Test Runner opener.
 */
export function Slide18InterludeATitle(_props: SlideProps) {
  return (
    <QuoteBreakout
      kicker="INTERLUDE A"
      quote="The tests were becoming hard to manage manually. So we built a real test runner."
    />
  );
}

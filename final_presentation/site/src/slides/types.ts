import type { ComponentType } from 'react';

/**
 * Every slide receives the same shape: its 1-based index and the total
 * deck length. Slides use this to paint the slide-number badge, but are
 * otherwise self-contained.
 */
export interface SlideProps {
  slideIndex: number;
  totalSlides: number;
}

export type SlideComponent = ComponentType<SlideProps>;

export type DeckAct =
  | 'ACT I · THE PITCH'
  | 'ACT II · PRODUCT DEMO'
  | 'ACT III · HOW WE BUILT IT'
  | 'ACT IV · TEAM & PROCESS'
  | 'ACT V · CLOSE';

export interface SlideEntry {
  id: string;
  title: string;
  act: DeckAct;
  speaker: string;
  component: SlideComponent;
}

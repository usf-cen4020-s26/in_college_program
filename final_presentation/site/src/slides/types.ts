import type { ComponentType } from 'react';

/**
 * Every slide receives its 1-based index, total deck length, and the
 * current build step (0-based). Slides use `step` to progressively
 * reveal content via StepReveal or manual checks.
 */
export interface SlideProps {
  slideIndex: number;
  totalSlides: number;
  /** Current build step within this slide (0-based). */
  step: number;
}

export type SlideComponent = ComponentType<SlideProps>;

export type DeckAct =
  | 'PART A · AGILE AT A GLANCE'
  | 'PART B · BUILDING INCOLLEGE'
  | 'INTERLUDE'
  | 'PART C · EPIC 9 DEEP DIVE'
  | 'PART D · ARCHITECTURE'
  | 'PART E · CLOSING';

export interface SlideEntry {
  id: string;
  title: string;
  act: DeckAct;
  speaker: string;
  component: SlideComponent;
  /** Number of build steps (default 1 = no sub-steps). */
  steps?: number;
  /** When true the slide fills the full viewport instead of the 16:9 stage. */
  breakout?: boolean;
}

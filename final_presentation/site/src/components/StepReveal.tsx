import type { ReactNode } from 'react';

interface StepRevealProps {
  /** The current step the slide is on (0-based). */
  currentStep: number;
  /** This content becomes visible when currentStep >= visibleAt. */
  visibleAt: number;
  children: ReactNode;
  className?: string;
}

/**
 * Wrapper that fades + slides children into view when the slide reaches
 * a given build step. Uses pure CSS transitions for smooth animation.
 */
export function StepReveal({ currentStep, visibleAt, children, className = '' }: StepRevealProps) {
  const visible = currentStep >= visibleAt;
  return (
    <div className={`step-reveal${visible ? ' visible' : ''} ${className}`}>
      {children}
    </div>
  );
}

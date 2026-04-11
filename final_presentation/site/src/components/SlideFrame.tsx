import type { ReactNode } from 'react';
import clsx from 'clsx';
import { Wordmark } from './Wordmark';

interface SlideFrameProps {
  act?: string;
  showWordmark?: boolean;
  kicker?: string;
  children: ReactNode;
  variant?: 'default' | 'hero' | 'quiet';
}

/**
 * Shared outer wrapper that every slide mounts. Paints the dark base
 * with a brand radial glow, optionally the act/section label and
 * wordmark in the top corners, an optional kicker above the content
 * area, and a thin accent rule across the bottom.
 *
 * Variants:
 *   - default: standard page padding and glow
 *   - hero: tighter padding, stronger glow (title / close slides)
 *   - quiet: minimal glow (burndown / image-heavy slides)
 */
export function SlideFrame({
  act,
  showWordmark = true,
  kicker,
  children,
  variant = 'default',
}: SlideFrameProps) {
  const glow =
    variant === 'hero'
      ? 'radial-gradient(ellipse 1600px 900px at 50% -200px, rgba(10,102,194,0.58) 0%, rgba(10,102,194,0) 72%)'
      : variant === 'quiet'
        ? 'radial-gradient(ellipse 1200px 520px at 50% -240px, rgba(10,102,194,0.22) 0%, rgba(10,102,194,0) 70%)'
        : 'radial-gradient(ellipse 1400px 700px at 50% -200px, rgba(10,102,194,0.45) 0%, rgba(10,102,194,0) 70%)';

  const paddingClass = clsx(
    'relative flex h-full w-full flex-col',
    variant === 'hero'
      ? 'px-[var(--spacing-page)] py-[clamp(32px,5%,72px)]'
      : 'p-[var(--spacing-page)]',
  );

  return (
    <div
      className="deck-slide"
      style={{ background: 'var(--color-bg-base)' }}
    >
      {/* Background glow layer */}
      <div
        aria-hidden
        className="pointer-events-none absolute inset-0"
        style={{ background: glow }}
      />

      {/* Content column */}
      <div className={paddingClass} style={{ gap: 'clamp(8px, 1em, 20px)' }}>
        {/* Top rail — compact */}
        {(act || showWordmark) && (
          <div className="flex w-full items-center justify-between" style={{ flexShrink: 0 }}>
            {act ? (
              <span
                className="font-medium uppercase"
                style={{
                  color: 'var(--color-text-dim)',
                  fontSize: 'clamp(8px, 0.75em, 12px)',
                  letterSpacing: '0.2em',
                  fontFamily: 'var(--font-body)',
                }}
              >
                {act}
              </span>
            ) : (
              <span />
            )}
            {showWordmark ? <Wordmark size={20} /> : <span />}
          </div>
        )}

        {/* Optional kicker */}
        {kicker && (
          <div
            style={{
              color: 'var(--color-brand-accent)',
              fontSize: 'clamp(10px, 0.875em, 14px)',
              fontFamily: 'var(--font-body)',
              letterSpacing: '0.16em',
              textTransform: 'uppercase',
              fontWeight: 600,
              flexShrink: 0,
            }}
          >
            {kicker}
          </div>
        )}

        {/* Body */}
        <div
          className="flex min-h-0 flex-1 flex-col"
          style={{ gap: 'clamp(8px, 1em, 20px)', overflow: 'hidden' }}
        >
          {children}
        </div>
      </div>

      {/* Bottom accent rule */}
      <div
        aria-hidden
        className="pointer-events-none absolute inset-x-0 bottom-0 h-px"
        style={{
          background:
            'linear-gradient(90deg, rgba(10,102,194,0) 0%, rgba(56,189,248,0.42) 50%, rgba(10,102,194,0) 100%)',
        }}
      />
    </div>
  );
}

import { SlideFrame } from './SlideFrame';

interface SectionTitleProps {
  /** e.g. "MILESTONE 1" or "INTERLUDE" */
  kicker: string;
  /** The main headline */
  title: string;
  /** e.g. "Jan 21 – Jan 27" */
  dateRange?: string;
  /** Optional epic number badge */
  epicNumber?: number;
  /** Act label for SlideFrame */
  act?: string;
}

/**
 * Full-slide section title card used at the start of each milestone
 * or interlude. Large centered text with a date range and kicker.
 */
export function SectionTitle({
  kicker,
  title,
  dateRange,
  epicNumber,
  act,
}: SectionTitleProps) {
  return (
    <SlideFrame act={act} variant="hero" showWordmark={false}>
      <div
        style={{
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'center',
          justifyContent: 'center',
          flex: 1,
          gap: 24,
          textAlign: 'center',
        }}
      >
        {/* Kicker line */}
        <div
          style={{
            display: 'flex',
            alignItems: 'center',
            gap: 16,
          }}
        >
          {epicNumber != null && (
            <span
              style={{
                display: 'inline-flex',
                alignItems: 'center',
                justifyContent: 'center',
                width: 'clamp(36px, 2.8em, 48px)',
                height: 'clamp(36px, 2.8em, 48px)',
                borderRadius: '50%',
                background: 'linear-gradient(135deg, #0A66C2, #38BDF8)',
                color: '#fff',
                fontFamily: 'var(--font-display)',
                fontSize: 'clamp(14px, 1.25em, 22px)',
                fontWeight: 700,
              }}
            >
              {epicNumber}
            </span>
          )}
          <span
            style={{
              fontFamily: 'var(--font-body)',
              fontSize: 'clamp(12px, 1em, 16px)',
              letterSpacing: '0.32em',
              textTransform: 'uppercase',
              color: 'var(--color-brand-accent)',
              fontWeight: 600,
            }}
          >
            {kicker}
          </span>
        </div>

        {/* Main headline */}
        <h1
          className="text-gradient"
          style={{
            margin: 0,
            fontFamily: 'var(--font-display)',
            fontSize: 'clamp(48px, 6.5em, 100px)',
            fontWeight: 700,
            letterSpacing: '-0.03em',
            lineHeight: 1,
            filter: 'drop-shadow(0 20px 60px rgba(10,102,194,0.35))',
          }}
        >
          {title}
        </h1>

        {/* Date range */}
        {dateRange && (
          <p
            style={{
              margin: 0,
              fontFamily: 'var(--font-mono)',
              fontSize: 'clamp(14px, 1.25em, 22px)',
              color: 'var(--color-text-muted)',
              letterSpacing: '0.06em',
            }}
          >
            {dateRange}
          </p>
        )}
      </div>
    </SlideFrame>
  );
}

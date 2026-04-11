import { SlideFrame } from './SlideFrame';

interface QuoteBreakoutProps {
  /** The interlude label, e.g. "INTERLUDE" */
  kicker?: string;
  /** The main quote text */
  quote: string;
  /** Optional subtitle below the quote */
  subtitle?: string;
}

/**
 * Full-screen quote slide used as the opening beat for interludes.
 * Large italic quote text centered on a deep glow background.
 */
export function QuoteBreakout({ kicker, quote, subtitle }: QuoteBreakoutProps) {
  return (
    <SlideFrame variant="hero" showWordmark={false} act="INTERLUDE">
      <style>{`
        @keyframes quote-fade-in {
          0% { opacity: 0; transform: translateY(20px); filter: blur(4px); }
          100% { opacity: 1; transform: translateY(0); filter: blur(0); }
        }
        .quote-text {
          animation: quote-fade-in 1200ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 200ms;
        }
        .quote-sub {
          animation: quote-fade-in 900ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 700ms;
        }
      `}</style>

      <div
        style={{
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'center',
          justifyContent: 'center',
          flex: 1,
          gap: 32,
          textAlign: 'center',
          padding: '0 5%',
        }}
      >
        {kicker && (
          <span
            style={{
              fontFamily: 'var(--font-body)',
              fontSize: 'clamp(11px, 0.875em, 14px)',
              letterSpacing: '0.32em',
              textTransform: 'uppercase',
              color: 'var(--color-brand-accent)',
              fontWeight: 600,
            }}
          >
            {kicker}
          </span>
        )}

        <blockquote
          className="quote-text"
          style={{
            margin: 0,
            fontFamily: 'var(--font-display)',
            fontSize: 'clamp(28px, 3.5em, 52px)',
            fontWeight: 400,
            fontStyle: 'italic',
            lineHeight: 1.3,
            color: 'var(--color-text-primary)',
            maxWidth: '90%',
          }}
        >
          "{quote}"
        </blockquote>

        {subtitle && (
          <p
            className="quote-sub"
            style={{
              margin: 0,
              fontFamily: 'var(--font-body)',
              fontSize: 'clamp(14px, 1.25em, 22px)',
              color: 'var(--color-text-muted)',
            }}
          >
            {subtitle}
          </p>
        )}
      </div>
    </SlideFrame>
  );
}

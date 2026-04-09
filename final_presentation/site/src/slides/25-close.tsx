import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { Wordmark } from '../components/Wordmark';

/**
 * Slide 25 — Closing Thank-You / Q&A slide. Hero variant, pure CSS
 * entry animations.
 */
export function Slide25Close(_props: SlideProps) {
  return (
    <SlideFrame variant="hero" showWordmark={false}>
      <style>{`
        @keyframes slide25-wordmark {
          0% { opacity: 0; transform: translateY(-12px); }
          100% { opacity: 1; transform: translateY(0); }
        }
        @keyframes slide25-thanks {
          0% { opacity: 0; transform: translateY(28px) scale(0.96); filter: blur(6px); }
          100% { opacity: 1; transform: translateY(0) scale(1); filter: blur(0); }
        }
        @keyframes slide25-questions {
          0% { opacity: 0; transform: translateY(14px); }
          100% { opacity: 1; transform: translateY(0); }
        }
        @keyframes slide25-signature {
          0% { opacity: 0; letter-spacing: 0.1em; }
          100% { opacity: 1; letter-spacing: 0.32em; }
        }
        @keyframes slide25-chip {
          0% { opacity: 0; transform: translateY(12px); }
          100% { opacity: 1; transform: translateY(0); }
        }
        .slide25-wordmark {
          animation: slide25-wordmark 800ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 80ms;
        }
        .slide25-thanks {
          animation: slide25-thanks 1200ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 240ms;
        }
        .slide25-questions {
          animation: slide25-questions 900ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 700ms;
        }
        .slide25-signature {
          animation: slide25-signature 1400ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 1000ms;
        }
        .slide25-chip {
          animation: slide25-chip 800ms cubic-bezier(0.16, 1, 0.3, 1) both;
        }
      `}</style>

      <div
        style={{
          flex: 1,
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'center',
          justifyContent: 'space-between',
          paddingTop: 8,
          paddingBottom: 8,
        }}
      >
        {/* Top — wordmark */}
        <div className="slide25-wordmark">
          <Wordmark size={48} />
        </div>

        {/* Center — hero */}
        <div
          style={{
            display: 'flex',
            flexDirection: 'column',
            alignItems: 'center',
            justifyContent: 'center',
            gap: 16,
            textAlign: 'center',
          }}
        >
          <h1
            className="slide25-thanks text-gradient"
            style={{
              margin: 0,
              fontFamily: 'var(--font-display)',
              fontWeight: 700,
              fontSize: 200,
              letterSpacing: '-0.035em',
              lineHeight: 0.95,
            }}
          >
            Thank You.
          </h1>
          <div
            className="slide25-questions"
            style={{
              margin: 0,
              fontFamily: 'var(--font-display)',
              fontWeight: 500,
              fontSize: 64,
              letterSpacing: '-0.02em',
              color: 'var(--color-text-muted)',
            }}
          >
            Questions?
          </div>

          <div
            className="slide25-signature"
            style={{
              marginTop: 18,
              fontFamily: 'var(--font-body)',
              fontSize: 14,
              fontWeight: 600,
              textTransform: 'uppercase',
              color: 'var(--color-text-dim)',
            }}
          >
            Trevor · Aaron · Olga · Melaine · Victoria
          </div>
        </div>

        {/* Bottom — link chips */}
        <div
          style={{
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            gap: 16,
            flexWrap: 'wrap',
          }}
        >
          <div
            className="slide25-chip"
            style={{
              animationDelay: '1200ms',
              display: 'inline-flex',
              alignItems: 'center',
              gap: 12,
              padding: '14px 22px',
              borderRadius: 999,
              background: 'var(--color-bg-panel)',
              border: '1px solid rgba(112,181,249,0.28)',
              boxShadow:
                '0 20px 40px -24px rgba(0,0,0,0.8), inset 0 1px 0 rgba(255,255,255,0.04)',
              fontFamily: 'var(--font-mono)',
              fontSize: 16,
              color: 'var(--color-text-primary)',
              letterSpacing: '0.01em',
            }}
          >
            <span
              aria-hidden
              style={{
                display: 'inline-flex',
                alignItems: 'center',
                justifyContent: 'center',
                width: 22,
                height: 22,
                borderRadius: '50%',
                background:
                  'linear-gradient(135deg, #0A66C2 0%, #38BDF8 100%)',
                color: '#F5F7FA',
                fontSize: 14,
                fontWeight: 700,
              }}
            >
              ↗
            </span>
            github.com/usf-cen4020-s26/in_college_program
          </div>

          <div
            className="slide25-chip"
            style={{
              animationDelay: '1340ms',
              display: 'inline-flex',
              alignItems: 'center',
              gap: 10,
              padding: '14px 22px',
              borderRadius: 999,
              background: 'var(--color-bg-panel)',
              border: '1px solid rgba(112,181,249,0.28)',
              boxShadow:
                '0 20px 40px -24px rgba(0,0,0,0.8), inset 0 1px 0 rgba(255,255,255,0.04)',
              fontFamily: 'var(--font-body)',
              fontSize: 15,
              fontWeight: 500,
              color: 'var(--color-text-muted)',
              letterSpacing: '0.02em',
            }}
          >
            Live deck · press
            <kbd
              style={{
                display: 'inline-flex',
                alignItems: 'center',
                justifyContent: 'center',
                minWidth: 24,
                height: 24,
                padding: '0 6px',
                borderRadius: 6,
                background: 'rgba(10,102,194,0.22)',
                border: '1px solid rgba(112,181,249,0.32)',
                color: 'var(--color-brand-accent)',
                fontFamily: 'var(--font-mono)',
                fontSize: 14,
                fontWeight: 600,
              }}
            >
              ←
            </kbd>
            <kbd
              style={{
                display: 'inline-flex',
                alignItems: 'center',
                justifyContent: 'center',
                minWidth: 24,
                height: 24,
                padding: '0 6px',
                borderRadius: 6,
                background: 'rgba(10,102,194,0.22)',
                border: '1px solid rgba(112,181,249,0.32)',
                color: 'var(--color-brand-accent)',
                fontFamily: 'var(--font-mono)',
                fontSize: 14,
                fontWeight: 600,
              }}
            >
              →
            </kbd>
            to navigate
          </div>
        </div>
      </div>
    </SlideFrame>
  );
}

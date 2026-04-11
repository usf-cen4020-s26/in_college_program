import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';

/**
 * Slide 63 — Thank You & Q&A closing slide.
 */
export function Slide63Close(_props: SlideProps) {
  return (
    <SlideFrame variant="hero" showWordmark={false}>
      <style>{`
        @keyframes slide63-fade-up {
          0% { opacity: 0; transform: translateY(32px); filter: blur(6px); }
          100% { opacity: 1; transform: translateY(0); filter: blur(0); }
        }
        @keyframes slide63-rise {
          0% { opacity: 0; transform: translateY(14px); }
          100% { opacity: 1; transform: translateY(0); }
        }
        @keyframes slide63-rule {
          0% { opacity: 0; transform: scaleX(0); }
          100% { opacity: 1; transform: scaleX(1); }
        }
        .slide63-headline {
          animation: slide63-fade-up 1200ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 200ms;
        }
        .slide63-rule {
          animation: slide63-rule 900ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 700ms;
          transform-origin: center;
        }
        .slide63-team {
          animation: slide63-rise 1100ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 900ms;
        }
        .slide63-qa {
          animation: slide63-rise 1100ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 1200ms;
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
        }}
      >
        <h1
          className="slide63-headline text-gradient"
          style={{
            margin: 0,
            fontFamily: 'var(--font-display)',
            fontSize: 'clamp(72px, 10em, 180px)',
            fontWeight: 700,
            letterSpacing: '-0.04em',
            lineHeight: 1,
            textAlign: 'center',
            filter: 'drop-shadow(0 30px 80px rgba(10,102,194,0.35))',
          }}
        >
          Thank You
        </h1>

        <div
          className="slide63-rule"
          style={{
            width: 180,
            height: 4,
            borderRadius: 2,
            background:
              'linear-gradient(90deg, rgba(10,102,194,0) 0%, #38BDF8 50%, rgba(10,102,194,0) 100%)',
          }}
        />

        <div
          className="slide63-team"
          style={{
            fontFamily: 'var(--font-body)',
            fontSize: 'clamp(13px, 1em, 16px)',
            fontWeight: 600,
            letterSpacing: '0.28em',
            textTransform: 'uppercase',
            color: 'var(--color-text-dim)',
            textAlign: 'center',
          }}
        >
          Trevor Flahardy&nbsp;&nbsp;&middot;&nbsp;&nbsp;Aaron Fraze&nbsp;&nbsp;&middot;&nbsp;&nbsp;Olga Druzhkova&nbsp;&nbsp;&middot;&nbsp;&nbsp;Melaine Fernandez Sarduy&nbsp;&nbsp;&middot;&nbsp;&nbsp;Victoria Field
        </div>

        <p
          className="slide63-qa"
          style={{
            margin: 0,
            fontFamily: 'var(--font-display)',
            fontSize: 'clamp(24px, 2.5em, 42px)',
            fontWeight: 400,
            color: 'var(--color-text-muted)',
            letterSpacing: '-0.01em',
          }}
        >
          Questions?
        </p>
      </div>
    </SlideFrame>
  );
}

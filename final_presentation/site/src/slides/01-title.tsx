import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';

/**
 * Hero opening slide. The gigantic "InCollege" wordmark acts as the
 * whole brand moment — no extra logo, just a gradient headline with a
 * fade+translate entry, a thin accent rule, and a tracked team strip at
 * the bottom.
 */
export function Slide01Title(_props: SlideProps) {
  return (
    <SlideFrame variant="hero" showWordmark={false}>
      <style>{`
        @keyframes slide01-fade-up {
          0% { opacity: 0; transform: translateY(32px); filter: blur(6px); }
          100% { opacity: 1; transform: translateY(0); filter: blur(0); }
        }
        @keyframes slide01-rise {
          0% { opacity: 0; transform: translateY(14px); }
          100% { opacity: 1; transform: translateY(0); }
        }
        @keyframes slide01-rule {
          0% { opacity: 0; transform: scaleX(0); }
          100% { opacity: 1; transform: scaleX(1); }
        }
        .slide01-label {
          animation: slide01-rise 900ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 120ms;
        }
        .slide01-headline {
          animation: slide01-fade-up 1200ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 280ms;
        }
        .slide01-subtitle {
          animation: slide01-rise 1100ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 720ms;
        }
        .slide01-rule {
          animation: slide01-rule 900ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 900ms;
          transform-origin: center;
        }
        .slide01-team {
          animation: slide01-rise 1200ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 1100ms;
        }
      `}</style>

      <div
        style={{
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'center',
          justifyContent: 'center',
          flex: 1,
          gap: 28,
          paddingTop: 40,
        }}
      >
        <div
          className="slide01-label"
          style={{
            fontFamily: 'var(--font-body)',
            fontSize: 16,
            letterSpacing: '0.32em',
            textTransform: 'uppercase',
            color: 'var(--color-text-dim)',
            fontWeight: 600,
          }}
        >
          CEN 4020 · Spring 2026 · InCollege VC Pitch
        </div>

        <h1
          className="slide01-headline text-gradient"
          style={{
            margin: 0,
            fontFamily: 'var(--font-display)',
            fontSize: 220,
            fontWeight: 700,
            letterSpacing: '-0.045em',
            lineHeight: 0.95,
            textAlign: 'center',
            filter: 'drop-shadow(0 30px 80px rgba(10,102,194,0.35))',
          }}
        >
          InCollege
        </h1>

        <div
          className="slide01-rule"
          style={{
            width: 180,
            height: 4,
            borderRadius: 2,
            background:
              'linear-gradient(90deg, rgba(10,102,194,0) 0%, #38BDF8 50%, rgba(10,102,194,0) 100%)',
          }}
        />

        <p
          className="slide01-subtitle"
          style={{
            margin: 0,
            fontFamily: 'var(--font-display)',
            fontSize: 38,
            fontWeight: 400,
            letterSpacing: '-0.01em',
            color: 'var(--color-text-muted)',
            textAlign: 'center',
            lineHeight: 1.25,
          }}
        >
          A career networking platform, built in{' '}
          <span style={{ color: 'var(--color-brand-accent)', fontWeight: 600 }}>COBOL</span>.
        </p>
      </div>

      <div
        className="slide01-team"
        style={{
          display: 'flex',
          justifyContent: 'center',
          fontFamily: 'var(--font-body)',
          fontSize: 13,
          fontWeight: 600,
          letterSpacing: '0.3em',
          textTransform: 'uppercase',
          color: 'var(--color-text-dim)',
          paddingBottom: 20,
        }}
      >
        Trevor Flahardy&nbsp;&nbsp;·&nbsp;&nbsp;Aaron Fraze&nbsp;&nbsp;·&nbsp;&nbsp;Olga Druzhkova&nbsp;&nbsp;·&nbsp;&nbsp;Melaine Fernandez Sarduy&nbsp;&nbsp;·&nbsp;&nbsp;Victoria Field
      </div>
    </SlideFrame>
  );
}

import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';

/**
 * Slide 1 — Hero opening. Large "InCollege" wordmark with staggered
 * fade+translate animations.
 */
export function Slide01Title(_props: SlideProps) {
  return (
    <SlideFrame variant="hero" showWordmark={false}>
      <style>{`
        @keyframes s01-fade-up {
          0% { opacity: 0; transform: translateY(32px); filter: blur(6px); }
          100% { opacity: 1; transform: translateY(0); filter: blur(0); }
        }
        @keyframes s01-rise {
          0% { opacity: 0; transform: translateY(14px); }
          100% { opacity: 1; transform: translateY(0); }
        }
        @keyframes s01-rule {
          0% { opacity: 0; transform: scaleX(0); }
          100% { opacity: 1; transform: scaleX(1); }
        }
        .s01-label { animation: s01-rise 900ms cubic-bezier(0.16,1,0.3,1) both 120ms; }
        .s01-headline { animation: s01-fade-up 1200ms cubic-bezier(0.16,1,0.3,1) both 280ms; }
        .s01-subtitle { animation: s01-rise 1100ms cubic-bezier(0.16,1,0.3,1) both 720ms; }
        .s01-rule { animation: s01-rule 900ms cubic-bezier(0.16,1,0.3,1) both 900ms; transform-origin: center; }
        .s01-team { animation: s01-rise 1200ms cubic-bezier(0.16,1,0.3,1) both 1100ms; }
      `}</style>

      <div
        style={{
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'center',
          justifyContent: 'center',
          flex: 1,
          gap: 'clamp(16px, 1.75em, 28px)',
          paddingTop: 40,
        }}
      >
        <div
          className="s01-label"
          style={{
            fontFamily: 'var(--font-body)',
            fontSize: 'clamp(10px, 1em, 16px)',
            letterSpacing: '0.32em',
            textTransform: 'uppercase',
            color: 'var(--color-text-dim)',
            fontWeight: 600,
          }}
        >
          CEN 4020 · Spring 2026 · Final Presentation
        </div>

        <h1
          className="s01-headline text-gradient"
          style={{
            margin: 0,
            fontFamily: 'var(--font-display)',
            fontSize: 'clamp(60px, 10em, 160px)',
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
          className="s01-rule"
          style={{
            width: 'clamp(80px, 11.25em, 180px)',
            height: 4,
            borderRadius: 2,
            background:
              'linear-gradient(90deg, rgba(10,102,194,0) 0%, #38BDF8 50%, rgba(10,102,194,0) 100%)',
          }}
        />

        <p
          className="s01-subtitle"
          style={{
            margin: 0,
            fontFamily: 'var(--font-display)',
            fontSize: 'clamp(20px, 2.375em, 38px)',
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
        className="s01-team"
        style={{
          display: 'flex',
          justifyContent: 'center',
          flexWrap: 'wrap',
          fontFamily: 'var(--font-body)',
          fontSize: 'clamp(9px, 0.8125em, 13px)',
          fontWeight: 600,
          letterSpacing: '0.3em',
          textTransform: 'uppercase',
          color: 'var(--color-text-dim)',
          paddingBottom: 20,
          gap: '0 0.5em',
        }}
      >
        Trevor Flahardy&nbsp;&nbsp;·&nbsp;&nbsp;Aaron Fraze&nbsp;&nbsp;·&nbsp;&nbsp;Olga
        Druzhkova&nbsp;&nbsp;·&nbsp;&nbsp;Melaine Fernandez Sarduy&nbsp;&nbsp;·&nbsp;&nbsp;Victoria
        Field
      </div>
    </SlideFrame>
  );
}

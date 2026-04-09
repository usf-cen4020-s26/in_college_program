import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';

/**
 * Thirty-second product pitch. The headline is a single sentence with
 * the middle word gradient-highlighted, followed by three bullet-free
 * statements that fade in with a stagger.
 */
const STATEMENTS = [
  {
    prefix: 'A full CRUD networking platform — ',
    emphasis: '9 epics,',
    suffix: ' end-to-end.',
  },
  {
    prefix: 'Persistent across restarts. ',
    emphasis: 'Zero cloud.',
    suffix: ' Seven DAT files.',
  },
  {
    prefix: '195 deterministic tests. ',
    emphasis: 'Zero mocks.',
    suffix: ' Real COBOL binary.',
  },
] as const;

export function Slide03Product(_props: SlideProps) {
  return (
    <SlideFrame act="ACT I · THE PITCH" kicker="THE PRODUCT">
      <style>{`
        @keyframes slide03-headline {
          0% { opacity: 0; transform: translateY(28px); filter: blur(4px); }
          100% { opacity: 1; transform: translateY(0); filter: blur(0); }
        }
        @keyframes slide03-line {
          0% { opacity: 0; transform: translateY(18px); }
          100% { opacity: 1; transform: translateY(0); }
        }
        .slide03-headline {
          animation: slide03-headline 1100ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 100ms;
        }
        .slide03-line {
          animation: slide03-line 900ms cubic-bezier(0.16, 1, 0.3, 1) both;
        }
      `}</style>

      <div
        style={{
          flex: 1,
          display: 'flex',
          flexDirection: 'column',
          justifyContent: 'center',
          gap: 80,
          paddingLeft: 40,
        }}
      >
        <h1
          className="slide03-headline"
          style={{
            margin: 0,
            fontFamily: 'var(--font-display)',
            fontWeight: 700,
            fontSize: 112,
            letterSpacing: '-0.035em',
            lineHeight: 0.98,
            color: 'var(--color-text-primary)',
            maxWidth: 1600,
          }}
        >
          Where{' '}
          <span className="text-gradient">college</span>{' '}
          meets career.
        </h1>

        <div
          style={{
            display: 'flex',
            flexDirection: 'column',
            gap: 24,
            maxWidth: 1500,
          }}
        >
          {STATEMENTS.map((s, i) => (
            <div
              key={s.emphasis}
              className="slide03-line"
              style={{
                display: 'flex',
                alignItems: 'baseline',
                gap: 20,
                animationDelay: `${700 + i * 220}ms`,
              }}
            >
              <span
                aria-hidden
                style={{
                  display: 'inline-block',
                  width: 10,
                  height: 10,
                  borderRadius: '50%',
                  background: 'var(--color-brand-accent)',
                  boxShadow: '0 0 18px rgba(112,181,249,0.6)',
                  flexShrink: 0,
                  transform: 'translateY(-6px)',
                }}
              />
              <p
                style={{
                  margin: 0,
                  fontFamily: 'var(--font-display)',
                  fontWeight: 500,
                  fontSize: 40,
                  lineHeight: 1.3,
                  letterSpacing: '-0.015em',
                  color: 'var(--color-text-muted)',
                }}
              >
                {s.prefix}
                <span style={{ color: 'var(--color-text-primary)', fontWeight: 700 }}>
                  {s.emphasis}
                </span>
                {s.suffix}
              </p>
            </div>
          ))}
        </div>
      </div>
    </SlideFrame>
  );
}

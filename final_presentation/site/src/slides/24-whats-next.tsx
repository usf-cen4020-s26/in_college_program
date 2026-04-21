import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { TEAM } from '../data/team';

/**
 * Slide 24 — Team reflection. A row of five cards (one per teammate)
 * each with a monogram, name, role chip, and a first-person quote.
 *
 * Quotes live inline here so team.ts stays about roles and contribution,
 * not narrative tone.
 */

const CARD_ACCENTS = ['#70B5F9', '#38BDF8', '#5EEAD4', '#A78BFA', '#F0ABFC'] as const;

const QUOTES: Record<string, string> = {
  'Trevor Flahardy':
    'Merging two PRs in 24 hours taught me that a well-run sprint is mostly careful listening.',
  'Aaron Fraze':
    '7840-VIEW-MESSAGES turned out elegant once I stopped fighting COBOL and let it recurse.',
  'Olga Druzhkova':
    'Display formatting is where the product becomes personal — tiny choices matter.',
  'Melaine Fernandez Sarduy':
    'Reusable seeds turned test-writing from chore to craft.',
  'Victoria Field':
    'Edge cases are where trust in the system is actually built.',
};

export function Slide24WhatsNext(_props: SlideProps) {
  return (
    <SlideFrame act="ACT V · CLOSE" kicker="WHAT WE LEARNED">
      <style>{`
        @keyframes slide24-headline {
          0% { opacity: 0; transform: translateY(18px); }
          100% { opacity: 1; transform: translateY(0); }
        }
        @keyframes slide24-card {
          0% { opacity: 0; transform: translateY(32px) scale(0.96); }
          100% { opacity: 1; transform: translateY(0) scale(1); }
        }
        .slide24-headline {
          animation: slide24-headline 900ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 80ms;
        }
        .slide24-card {
          animation: slide24-card 900ms cubic-bezier(0.16, 1, 0.3, 1) both;
        }
      `}</style>

      <h1
        className="slide24-headline"
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontWeight: 700,
          fontSize: 84,
          letterSpacing: '-0.025em',
          lineHeight: 1.02,
          color: 'var(--color-text-primary)',
        }}
      >
        Five voices,{' '}
        <span className="text-gradient">one deck.</span>
      </h1>

      <div
        style={{
          flex: 1,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
        }}
      >
        <div
          style={{
            display: 'flex',
            flexWrap: 'nowrap',
            justifyContent: 'center',
            gap: 24,
            maxWidth: 1760,
          }}
        >
          {TEAM.map((member, i) => {
            const accent = CARD_ACCENTS[i % CARD_ACCENTS.length];
            const quote = QUOTES[member.name] ?? '';
            return (
              <div
                key={member.name}
                className="slide24-card"
                style={{
                  animationDelay: `${260 + i * 180}ms`,
                  width: 318,
                  minHeight: 440,
                  padding: '30px 28px 32px',
                  borderRadius: 22,
                  background: 'var(--color-bg-panel)',
                  border: '1px solid rgba(112,181,249,0.16)',
                  boxShadow:
                    '0 36px 72px -36px rgba(0,0,0,0.85), inset 0 1px 0 rgba(255,255,255,0.04)',
                  display: 'flex',
                  flexDirection: 'column',
                  alignItems: 'flex-start',
                  gap: 18,
                }}
              >
                <div
                  style={{
                    width: 60,
                    height: 60,
                    borderRadius: '50%',
                    background: `linear-gradient(135deg, #0A66C2 0%, ${accent} 100%)`,
                    display: 'flex',
                    alignItems: 'center',
                    justifyContent: 'center',
                    color: '#F5F7FA',
                    fontFamily: 'var(--font-display)',
                    fontWeight: 700,
                    fontSize: 22,
                    letterSpacing: '-0.01em',
                    boxShadow: '0 14px 28px -14px rgba(10,102,194,0.75)',
                  }}
                >
                  {member.initials}
                </div>

                <div
                  style={{
                    fontFamily: 'var(--font-display)',
                    fontWeight: 700,
                    fontSize: 22,
                    letterSpacing: '-0.02em',
                    color: 'var(--color-text-primary)',
                    lineHeight: 1.15,
                  }}
                >
                  {member.name}
                </div>

                <div
                  style={{
                    display: 'inline-flex',
                    padding: '6px 12px',
                    borderRadius: 999,
                    background: 'rgba(10,102,194,0.22)',
                    border: '1px solid rgba(112,181,249,0.28)',
                    color: 'var(--color-brand-accent)',
                    fontFamily: 'var(--font-body)',
                    fontSize: 11,
                    fontWeight: 600,
                    textTransform: 'uppercase',
                    letterSpacing: '0.14em',
                  }}
                >
                  {member.role}
                </div>

                <div
                  aria-hidden
                  style={{
                    width: 48,
                    height: 3,
                    borderRadius: 2,
                    background: `linear-gradient(90deg, #0A66C2 0%, ${accent} 100%)`,
                    marginTop: 4,
                  }}
                />

                <p
                  style={{
                    margin: 0,
                    fontFamily: 'var(--font-body)',
                    fontSize: 17,
                    lineHeight: 1.5,
                    color: 'var(--color-text-primary)',
                    fontStyle: 'italic',
                  }}
                >
                  <span
                    aria-hidden
                    style={{
                      color: accent,
                      fontFamily: 'var(--font-display)',
                      fontSize: 28,
                      lineHeight: 0,
                      marginRight: 4,
                      verticalAlign: '-0.15em',
                    }}
                  >
                    “
                  </span>
                  {quote}
                </p>
              </div>
            );
          })}
        </div>
      </div>
    </SlideFrame>
  );
}

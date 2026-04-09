import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { PersonCard } from '../components/PersonCard';
import { TEAM } from '../data/team';

/**
 * The team slide. Staggered entry on the five person cards arranged in
 * a centered row so everyone reads left-to-right in speaking order.
 *
 * Each card uses a slightly different accent hue so the row feels alive
 * without escaping the brand palette.
 */
const CARD_ACCENTS = ['#70B5F9', '#38BDF8', '#5EEAD4', '#A78BFA', '#F0ABFC'] as const;

export function Slide02Team(_props: SlideProps) {
  return (
    <SlideFrame act="ACT I · THE PITCH" kicker="THE TEAM">
      <style>{`
        @keyframes slide02-headline {
          0% { opacity: 0; transform: translateY(24px); }
          100% { opacity: 1; transform: translateY(0); }
        }
        @keyframes slide02-card {
          0% { opacity: 0; transform: translateY(36px) scale(0.96); }
          100% { opacity: 1; transform: translateY(0) scale(1); }
        }
        @keyframes slide02-caption {
          0% { opacity: 0; }
          100% { opacity: 1; }
        }
        .slide02-headline {
          animation: slide02-headline 900ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 80ms;
        }
        .slide02-card {
          animation: slide02-card 900ms cubic-bezier(0.16, 1, 0.3, 1) both;
        }
        .slide02-caption {
          animation: slide02-caption 1000ms ease-out both;
          animation-delay: 1200ms;
        }
      `}</style>

      <h1
        className="slide02-headline"
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
        Five people.{' '}
        <span className="text-gradient">One pitch.</span>
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
            flexWrap: 'wrap',
            justifyContent: 'center',
            gap: 28,
            rowGap: 32,
            maxWidth: 1760,
          }}
        >
          {TEAM.map((member, i) => (
            <div
              key={member.name}
              className="slide02-card"
              style={{
                animationDelay: `${260 + i * 140}ms`,
              }}
            >
              <PersonCard
                name={member.name}
                role={member.role}
                initials={member.initials}
                accent={CARD_ACCENTS[i % CARD_ACCENTS.length]}
              />
            </div>
          ))}
        </div>
      </div>

      <div
        className="slide02-caption"
        style={{
          textAlign: 'center',
          fontFamily: 'var(--font-body)',
          fontSize: 18,
          color: 'var(--color-text-muted)',
          letterSpacing: '0.02em',
        }}
      >
        Every member presents at least twice.
      </div>
    </SlideFrame>
  );
}

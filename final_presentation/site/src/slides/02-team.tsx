import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { PersonCard } from '../components/PersonCard';
import { TEAM } from '../data/team';

const ACCENTS = ['#0A66C2', '#38BDF8', '#22C55E', '#F59E0B', '#A78BFA'];

/**
 * Slide 2 — Meet the Team. Five PersonCards with staggered fade-in.
 */
export function Slide02Team(_props: SlideProps) {
  return (
    <SlideFrame act="PART A · AGILE AT A GLANCE" kicker="THE TEAM">
      <style>{`
        @keyframes s02-card {
          0% { opacity: 0; transform: translateY(36px) scale(0.96); }
          100% { opacity: 1; transform: translateY(0) scale(1); }
        }
        .s02-card { animation: s02-card 900ms cubic-bezier(0.16,1,0.3,1) both; }
      `}</style>

      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(36px, 4em, 64px)',
          fontWeight: 700,
          letterSpacing: '-0.025em',
          lineHeight: 1.02,
          color: 'var(--color-text-primary)',
        }}
      >
        Meet the <span className="text-gradient">Team.</span>
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
            gap: 'clamp(16px, 1.75em, 28px)',
            rowGap: 'clamp(20px, 2em, 32px)',
            maxWidth: '100%',
          }}
        >
          {TEAM.map((member, i) => (
            <div
              key={member.name}
              className="s02-card"
              style={{ animationDelay: `${260 + i * 140}ms` }}
            >
              <PersonCard
                name={member.name}
                role={member.role}
                initials={member.initials}
                accent={ACCENTS[i % ACCENTS.length]}
              />
            </div>
          ))}
        </div>
      </div>

      <div
        style={{
          textAlign: 'center',
          fontFamily: 'var(--font-body)',
          fontSize: 'clamp(12px, 1.125em, 18px)',
          color: 'var(--color-text-muted)',
          letterSpacing: '0.02em',
        }}
      >
        Roles rotated every epic — everyone leads, codes, and tests.
      </div>
    </SlideFrame>
  );
}

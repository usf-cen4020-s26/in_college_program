import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';

const IDEAS = [
  { title: 'Real Database', detail: 'Replace .DAT files with SQLite or similar for true relational queries.' },
  { title: 'Graphical Interface', detail: 'Web or TUI frontend to move beyond line-mode interaction.' },
  { title: 'Push Notifications', detail: 'Real-time message alerts instead of polling the inbox.' },
  { title: 'Expanded Network', detail: 'Groups, endorsements, and recommendations for richer connections.' },
];

/**
 * Slide 62 — What's Next.
 */
export function Slide62WhatsNext(_props: SlideProps) {
  return (
    <SlideFrame act="PART E · CLOSING" kicker="FUTURE">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(48px, 5.5em, 88px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          lineHeight: 1.05,
        }}
      >
        <span className="text-gradient">What's Next</span>
      </h1>

      <div
        style={{
          flex: 1,
          display: 'grid',
          gridTemplateColumns: '1fr 1fr',
          gridTemplateRows: '1fr 1fr',
          gap: 24,
          alignContent: 'center',
        }}
      >
        {IDEAS.map((idea) => (
          <div
            key={idea.title}
            style={{
              padding: '32px 34px',
              borderRadius: 20,
              background: 'var(--color-bg-panel)',
              border: '1px solid rgba(112,181,249,0.12)',
              boxShadow:
                '0 24px 48px -28px rgba(0,0,0,0.75), inset 0 1px 0 rgba(255,255,255,0.04)',
              display: 'flex',
              flexDirection: 'column',
              gap: 14,
            }}
          >
            <div
              style={{
                fontFamily: 'var(--font-display)',
                fontWeight: 700,
                fontSize: 'clamp(20px, 1.6em, 28px)',
                color: 'var(--color-text-primary)',
                letterSpacing: '-0.01em',
              }}
            >
              {idea.title}
            </div>
            <p
              style={{
                margin: 0,
                fontFamily: 'var(--font-body)',
                fontSize: 'clamp(14px, 1.1em, 19px)',
                lineHeight: 1.55,
                color: 'var(--color-text-muted)',
              }}
            >
              {idea.detail}
            </p>
          </div>
        ))}
      </div>
    </SlideFrame>
  );
}

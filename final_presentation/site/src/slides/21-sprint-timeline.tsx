import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { asset } from '../lib/asset';

/**
 * Slide 21 — How the nine-day Epic #9 sprint played out.
 *
 * Two-column layout: a vertical Apr 1 → Apr 9 milestone timeline on the
 * left (with staggered CSS fade-ins) and a framed screenshot of the
 * GitHub commit list on the right.
 */

interface TimelineEvent {
  date: string;
  body: string;
}

const EVENTS: readonly TimelineEvent[] = [
  {
    date: 'Apr 1–2',
    body: 'Epic #8 (Send Messages) wraps, PR #29 merged. Epic #9 scoped.',
  },
  {
    date: 'Apr 3',
    body: '`impl/view_messages` branch cut. Aaron lands `7840-VIEW-MESSAGES`. Olga lands MSW-466/467 formatting.',
  },
  {
    date: 'Apr 4–5',
    body: 'Victoria writes 24 view_message fixtures (positive / negative / edge / persistence).',
  },
  {
    date: 'Apr 6',
    body: 'Melaine wires @seed_message / @seed_connection into the harness.',
  },
  {
    date: 'Apr 7',
    body: 'PR #35 — impl/view_messages merged into main.',
  },
  {
    date: 'Apr 8',
    body: 'PR #36 — test/view_messages merged. WS-variable fix lands.',
  },
  {
    date: 'Apr 9',
    body: 'Sprint closes. 195 fixtures green. Deck goes live.',
  },
] as const;

// Splits the body text on backtick pairs and renders the contents of
// each pair as an inline mono snippet. Keeps the timeline text readable
// while still letting branch names / paragraph names pop.
function renderBody(body: string) {
  const parts = body.split('`');
  return parts.map((part, i) =>
    i % 2 === 1 ? (
      <span
        key={i}
        style={{
          fontFamily: 'var(--font-mono)',
          fontSize: 17,
          padding: '1px 8px',
          borderRadius: 6,
          background: 'rgba(10,102,194,0.18)',
          border: '1px solid rgba(112,181,249,0.22)',
          color: 'var(--color-brand-accent)',
        }}
      >
        {part}
      </span>
    ) : (
      <span key={i}>{part}</span>
    ),
  );
}

export function Slide21SprintTimeline(_props: SlideProps) {
  return (
    <SlideFrame act="ACT IV · TEAM & PROCESS" kicker="THE SPRINT">
      <style>{`
        @keyframes slide21-headline {
          0% { opacity: 0; transform: translateY(18px); }
          100% { opacity: 1; transform: translateY(0); }
        }
        @keyframes slide21-row {
          0% { opacity: 0; transform: translateX(-18px); }
          100% { opacity: 1; transform: translateX(0); }
        }
        @keyframes slide21-frame {
          0% { opacity: 0; transform: translateY(22px) scale(0.98); }
          100% { opacity: 1; transform: translateY(0) scale(1); }
        }
        .slide21-headline {
          animation: slide21-headline 900ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 80ms;
        }
        .slide21-row {
          animation: slide21-row 720ms cubic-bezier(0.16, 1, 0.3, 1) both;
        }
        .slide21-frame {
          animation: slide21-frame 1000ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 260ms;
        }
      `}</style>

      <h1
        className="slide21-headline"
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontWeight: 700,
          fontSize: 72,
          letterSpacing: '-0.025em',
          lineHeight: 1.04,
          color: 'var(--color-text-primary)',
        }}
      >
        How nine days of{' '}
        <span className="text-gradient">Epic #9</span> played out.
      </h1>

      <div
        style={{
          flex: 1,
          display: 'grid',
          gridTemplateColumns: '55fr 45fr',
          gap: 56,
          alignItems: 'stretch',
          minHeight: 0,
        }}
      >
        {/* LEFT — vertical timeline */}
        <div
          style={{
            position: 'relative',
            display: 'flex',
            flexDirection: 'column',
            justifyContent: 'space-between',
            paddingLeft: 36,
            paddingTop: 6,
            paddingBottom: 6,
          }}
        >
          {/* vertical rail */}
          <div
            aria-hidden
            style={{
              position: 'absolute',
              left: 11,
              top: 14,
              bottom: 14,
              width: 2,
              background:
                'linear-gradient(180deg, rgba(56,189,248,0) 0%, rgba(56,189,248,0.55) 12%, rgba(56,189,248,0.55) 88%, rgba(56,189,248,0) 100%)',
            }}
          />

          {EVENTS.map((event, i) => (
            <div
              key={event.date}
              className="slide21-row"
              style={{
                position: 'relative',
                display: 'flex',
                flexDirection: 'column',
                gap: 4,
                animationDelay: `${260 + i * 120}ms`,
              }}
            >
              {/* milestone dot */}
              <div
                aria-hidden
                style={{
                  position: 'absolute',
                  left: -36,
                  top: 8,
                  width: 24,
                  height: 24,
                  borderRadius: '50%',
                  background:
                    'radial-gradient(circle at 30% 30%, #70B5F9 0%, #0A66C2 70%)',
                  border: '2px solid rgba(5,8,20,1)',
                  boxShadow: '0 0 0 2px rgba(112,181,249,0.35), 0 0 18px rgba(56,189,248,0.45)',
                }}
              />
              <div
                style={{
                  fontFamily: 'var(--font-display)',
                  fontWeight: 700,
                  fontSize: 20,
                  letterSpacing: '0.04em',
                  textTransform: 'uppercase',
                  color: 'var(--color-brand-accent)',
                }}
              >
                {event.date}
              </div>
              <div
                style={{
                  fontFamily: 'var(--font-body)',
                  fontSize: 19,
                  lineHeight: 1.45,
                  color: 'var(--color-text-primary)',
                  maxWidth: 720,
                }}
              >
                {renderBody(event.body)}
              </div>
            </div>
          ))}
        </div>

        {/* RIGHT — framed github screenshot */}
        <div
          className="slide21-frame"
          style={{
            display: 'flex',
            flexDirection: 'column',
            alignItems: 'center',
            justifyContent: 'center',
            gap: 18,
            minHeight: 0,
          }}
        >
          <div
            style={{
              width: '100%',
              maxHeight: '100%',
              padding: 10,
              borderRadius: 16,
              background: 'var(--color-bg-panel)',
              border: '1px solid rgba(112,181,249,0.28)',
              boxShadow:
                '0 40px 80px -40px rgba(0,0,0,0.9), 0 0 0 1px rgba(10,102,194,0.18), inset 0 1px 0 rgba(255,255,255,0.04)',
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
              overflow: 'hidden',
            }}
          >
            <img
              src={asset('assets/github.png')}
              alt="GitHub commit list, April 1-9 2026"
              style={{
                display: 'block',
                maxWidth: '100%',
                maxHeight: '100%',
                objectFit: 'contain',
                borderRadius: 8,
              }}
            />
          </div>
          <div
            style={{
              fontFamily: 'var(--font-body)',
              fontSize: 15,
              color: 'var(--color-text-muted)',
              letterSpacing: '0.02em',
              textAlign: 'center',
            }}
          >
            Source: repo commit history, Apr 1–9 2026.
          </div>
        </div>
      </div>
    </SlideFrame>
  );
}

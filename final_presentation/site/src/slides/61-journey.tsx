import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';

const EPICS = [
  { num: 1, label: 'Auth' },
  { num: 2, label: 'Profile' },
  { num: 3, label: 'Search' },
  { num: 4, label: 'Connect' },
  { num: 5, label: 'Jobs' },
  { num: 6, label: 'Apply' },
  { num: 7, label: 'My Apps' },
  { num: 8, label: 'Send Msg' },
  { num: 9, label: 'View Msg' },
];

/**
 * Slide 61 — The Journey Timeline.
 */
export function Slide61Journey(_props: SlideProps) {
  return (
    <SlideFrame act="PART E · CLOSING" kicker="THE JOURNEY">
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
        <span className="text-gradient">11 Weeks, 9 Epics</span>
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
            width: '100%',
            maxWidth: 1600,
            position: 'relative',
            display: 'flex',
            flexDirection: 'column',
            gap: 32,
          }}
        >
          {/* Date labels */}
          <div
            style={{
              display: 'flex',
              justifyContent: 'space-between',
              fontFamily: 'var(--font-mono)',
              fontSize: 'clamp(13px, 1em, 16px)',
              color: 'var(--color-text-muted)',
            }}
          >
            <span>Jan 21</span>
            <span>Apr 8</span>
          </div>

          {/* Timeline line */}
          <div style={{ position: 'relative' }}>
            <div
              style={{
                position: 'absolute',
                top: '50%',
                left: 0,
                right: 0,
                height: 4,
                borderRadius: 2,
                background: 'linear-gradient(90deg, #0A66C2, #38BDF8)',
                transform: 'translateY(-50%)',
              }}
            />

            {/* Dots */}
            <div
              style={{
                display: 'flex',
                justifyContent: 'space-between',
                position: 'relative',
              }}
            >
              {EPICS.map((epic) => (
                <div
                  key={epic.num}
                  style={{
                    display: 'flex',
                    flexDirection: 'column',
                    alignItems: 'center',
                    gap: 14,
                  }}
                >
                  {/* Dot */}
                  <div
                    style={{
                      width: 44,
                      height: 44,
                      borderRadius: '50%',
                      background: 'linear-gradient(135deg, #0A66C2, #38BDF8)',
                      display: 'flex',
                      alignItems: 'center',
                      justifyContent: 'center',
                      fontFamily: 'var(--font-display)',
                      fontWeight: 700,
                      fontSize: 18,
                      color: '#fff',
                      boxShadow: '0 8px 24px -8px rgba(10,102,194,0.5)',
                    }}
                  >
                    {epic.num}
                  </div>

                  {/* Label */}
                  <span
                    style={{
                      fontFamily: 'var(--font-body)',
                      fontSize: 'clamp(11px, 0.85em, 14px)',
                      color: 'var(--color-text-muted)',
                      textAlign: 'center',
                      whiteSpace: 'nowrap',
                    }}
                  >
                    {epic.label}
                  </span>
                </div>
              ))}
            </div>
          </div>
        </div>
      </div>
    </SlideFrame>
  );
}

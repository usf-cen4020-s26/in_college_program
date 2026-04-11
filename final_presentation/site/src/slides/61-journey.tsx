import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';

const TOP_ROW = [
  { num: 1, label: 'Authentication', date: 'Jan 21' },
  { num: 2, label: 'Profiles', date: 'Feb 1' },
  { num: 3, label: 'Search', date: 'Feb 12' },
  { num: 4, label: 'Connections', date: 'Feb 19' },
  { num: 5, label: 'Network', date: 'Mar 5' },
] as const;

const BOTTOM_ROW = [
  { num: 9, label: 'View Messages', date: 'Apr 8' },
  { num: 8, label: 'Send Messages', date: 'Apr 2' },
  { num: 7, label: 'Browse & Apply', date: 'Mar 25' },
  { num: 6, label: 'Job Posting', date: 'Mar 12' },
] as const;

/**
 * Slide 61 — The Journey. Snake/zigzag timeline:
 * 5 across the top (left → right), connector drops down,
 * 4 across the bottom (right → left).
 */
export function Slide61Journey(_props: SlideProps) {
  return (
    <SlideFrame act="PART E · CLOSING" kicker="THE JOURNEY">
      <style>{`
        @keyframes s61-pop {
          0% { opacity: 0; transform: scale(0.6) translateY(12px); }
          100% { opacity: 1; transform: scale(1) translateY(0); }
        }
        @keyframes s61-line-h {
          0% { transform: scaleX(0); }
          100% { transform: scaleX(1); }
        }
        @keyframes s61-line-v {
          0% { transform: scaleY(0); }
          100% { transform: scaleY(1); }
        }
      `}</style>

      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(28px, 3.5em, 56px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
          textAlign: 'center',
          flexShrink: 0,
        }}
      >
        11 Weeks, <span className="text-gradient">9 Epics.</span>
      </h1>

      <div
        style={{
          flex: 1,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          minHeight: 0,
        }}
      >
        <div style={{ width: '92%', maxWidth: 1400 }}>
          {/* ── Top row: Epics 1-5 (left → right) ── */}
          <div style={{ position: 'relative' }}>
            {/* Connecting line */}
            <div
              style={{
                position: 'absolute',
                top: 'clamp(20px, 2.25em, 36px)',
                left: 'clamp(20px, 2.25em, 36px)',
                right: 'clamp(20px, 2.25em, 36px)',
                height: 3,
                borderRadius: 2,
                background: 'linear-gradient(90deg, #0A66C2, #38BDF8)',
                opacity: 0.4,
                transformOrigin: 'left',
                animation: 's61-line-h 800ms cubic-bezier(0.16,1,0.3,1) both 100ms',
              }}
            />
            <div style={{ display: 'flex', justifyContent: 'space-between', position: 'relative' }}>
              {TOP_ROW.map((epic, i) => (
                <EpicNode key={epic.num} epic={epic} delay={200 + i * 120} />
              ))}
            </div>
          </div>

          {/* ── Vertical connector (right side, from epic 5 down to epic 6 area) ── */}
          <div
            style={{
              display: 'flex',
              justifyContent: 'flex-end',
              paddingRight: 'clamp(14px, 1.5em, 28px)',
            }}
          >
            <div
              style={{
                width: 3,
                height: 'clamp(28px, 3em, 48px)',
                borderRadius: 2,
                background: 'linear-gradient(180deg, #38BDF8, #85C5FF)',
                opacity: 0.4,
                transformOrigin: 'top',
                animation: 's61-line-v 500ms cubic-bezier(0.16,1,0.3,1) both 850ms',
              }}
            />
          </div>

          {/* ── Bottom row: Epics 9-6 (right → left, displayed reversed) ── */}
          <div style={{ position: 'relative' }}>
            {/* Connecting line */}
            <div
              style={{
                position: 'absolute',
                top: 'clamp(20px, 2.25em, 36px)',
                left: 'calc(12.5% + clamp(10px, 1em, 18px))',
                right: 'clamp(20px, 2.25em, 36px)',
                height: 3,
                borderRadius: 2,
                background: 'linear-gradient(90deg, #85C5FF, #38BDF8)',
                opacity: 0.4,
                transformOrigin: 'right',
                animation: 's61-line-h 800ms cubic-bezier(0.16,1,0.3,1) both 1000ms',
              }}
            />
            <div
              style={{
                display: 'flex',
                justifyContent: 'space-between',
                position: 'relative',
                paddingLeft: '12.5%',
              }}
            >
              {BOTTOM_ROW.map((epic, i) => (
                <EpicNode key={epic.num} epic={epic} delay={1100 + i * 120} />
              ))}
            </div>
          </div>
        </div>
      </div>
    </SlideFrame>
  );
}

function EpicNode({
  epic,
  delay,
}: {
  epic: { num: number; label: string; date: string };
  delay: number;
}) {
  return (
    <div
      style={{
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
        gap: 'clamp(6px, 0.6em, 10px)',
        animation: `s61-pop 600ms cubic-bezier(0.16,1,0.3,1) both ${delay}ms`,
      }}
    >
      {/* Circle */}
      <div
        style={{
          width: 'clamp(36px, 4.5em, 72px)',
          height: 'clamp(36px, 4.5em, 72px)',
          borderRadius: '50%',
          background: 'linear-gradient(135deg, #0A66C2, #38BDF8)',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          color: '#fff',
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(14px, 1.5em, 24px)',
          fontWeight: 700,
          boxShadow: '0 6px 24px rgba(10,102,194,0.45)',
          flexShrink: 0,
        }}
      >
        {epic.num}
      </div>

      {/* Label */}
      <span
        style={{
          fontFamily: 'var(--font-body)',
          fontSize: 'clamp(9px, 0.75em, 13px)',
          fontWeight: 600,
          color: 'var(--color-text-primary)',
          textAlign: 'center',
          whiteSpace: 'nowrap',
        }}
      >
        {epic.label}
      </span>

      {/* Date */}
      <span
        style={{
          fontFamily: 'var(--font-mono)',
          fontSize: 'clamp(9px, 0.7em, 12px)',
          color: 'var(--color-text-dim)',
          textAlign: 'center',
          whiteSpace: 'nowrap',
        }}
      >
        {epic.date}
      </span>
    </div>
  );
}

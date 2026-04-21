import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { asset } from '../lib/asset';

/**
 * Slide 23 — Burndown at sprint end. Large framed chart with three
 * annotation chips beneath it that trace the staircase drops.
 */

const ANNOTATIONS: readonly string[] = [
  'Apr 6 → 7 — impl/view_messages merges (PR #35)',
  'Apr 7 → 8 — test/view_messages merges (PR #36)',
  'Apr 9 — zero remaining · sprint closes',
] as const;

export function Slide23BurndownEnd(_props: SlideProps) {
  return (
    <SlideFrame
      act="ACT IV · TEAM & PROCESS"
      kicker="BURNDOWN · END OF SPRINT"
      variant="quiet"
    >
      <style>{`
        @keyframes slide23-headline {
          0% { opacity: 0; transform: translateY(18px); }
          100% { opacity: 1; transform: translateY(0); }
        }
        @keyframes slide23-frame {
          0% { opacity: 0; transform: translateY(26px) scale(0.985); }
          100% { opacity: 1; transform: translateY(0) scale(1); }
        }
        @keyframes slide23-chip {
          0% { opacity: 0; transform: translateY(14px); }
          100% { opacity: 1; transform: translateY(0); }
        }
        .slide23-headline {
          animation: slide23-headline 900ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 80ms;
        }
        .slide23-frame {
          animation: slide23-frame 1100ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 260ms;
        }
        .slide23-chip {
          animation: slide23-chip 700ms cubic-bezier(0.16, 1, 0.3, 1) both;
        }
      `}</style>

      <h1
        className="slide23-headline"
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
        <span className="text-gradient">Zero remaining</span> by Thursday.
      </h1>

      <div
        style={{
          flex: 1,
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'center',
          justifyContent: 'center',
          gap: 28,
          minHeight: 0,
        }}
      >
        <div
          className="slide23-frame"
          style={{
            width: '75%',
            maxHeight: '100%',
            padding: 14,
            borderRadius: 18,
            background: 'var(--color-bg-panel)',
            border: '1px solid rgba(112,181,249,0.26)',
            boxShadow:
              '0 50px 100px -40px rgba(0,0,0,0.9), 0 0 0 1px rgba(10,102,194,0.16), inset 0 1px 0 rgba(255,255,255,0.04)',
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            overflow: 'hidden',
          }}
        >
          <img
            src={asset('assets/burndown_end.png')}
            alt="Sprint end burndown chart, Thursday April 9 2026"
            style={{
              display: 'block',
              maxWidth: '100%',
              maxHeight: '100%',
              objectFit: 'contain',
              borderRadius: 10,
            }}
          />
        </div>

        <div
          style={{
            display: 'flex',
            flexWrap: 'wrap',
            justifyContent: 'center',
            gap: 16,
            maxWidth: 1600,
          }}
        >
          {ANNOTATIONS.map((label, i) => (
            <div
              key={label}
              className="slide23-chip"
              style={{
                animationDelay: `${900 + i * 180}ms`,
                display: 'inline-flex',
                alignItems: 'center',
                gap: 10,
                padding: '12px 20px',
                borderRadius: 999,
                background:
                  'linear-gradient(135deg, rgba(10,102,194,0.32) 0%, rgba(56,189,248,0.22) 100%)',
                border: '1px solid rgba(112,181,249,0.45)',
                color: 'var(--color-text-primary)',
                fontFamily: 'var(--font-body)',
                fontSize: 16,
                fontWeight: 600,
                letterSpacing: '0.01em',
                boxShadow: '0 14px 28px -18px rgba(10,102,194,0.6)',
              }}
            >
              <span
                aria-hidden
                style={{
                  width: 8,
                  height: 8,
                  borderRadius: '50%',
                  background: 'var(--color-brand-accent)',
                  boxShadow: '0 0 10px rgba(112,181,249,0.8)',
                }}
              />
              {label}
            </div>
          ))}
        </div>
      </div>
    </SlideFrame>
  );
}

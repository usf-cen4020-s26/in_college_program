import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { asset } from '../lib/asset';

/**
 * Slide 22 — Burndown on sprint Monday. Big framed chart, quiet chrome.
 */
export function Slide22BurndownStart(_props: SlideProps) {
  return (
    <SlideFrame
      act="ACT IV · TEAM & PROCESS"
      kicker="BURNDOWN · MONDAY APR 3"
      variant="quiet"
    >
      <style>{`
        @keyframes slide22-headline {
          0% { opacity: 0; transform: translateY(18px); }
          100% { opacity: 1; transform: translateY(0); }
        }
        @keyframes slide22-frame {
          0% { opacity: 0; transform: translateY(26px) scale(0.985); }
          100% { opacity: 1; transform: translateY(0) scale(1); }
        }
        @keyframes slide22-caption {
          0% { opacity: 0; }
          100% { opacity: 1; }
        }
        .slide22-headline {
          animation: slide22-headline 900ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 80ms;
        }
        .slide22-frame {
          animation: slide22-frame 1100ms cubic-bezier(0.16, 1, 0.3, 1) both;
          animation-delay: 260ms;
        }
        .slide22-caption {
          animation: slide22-caption 900ms ease-out both;
          animation-delay: 900ms;
        }
      `}</style>

      <h1
        className="slide22-headline"
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
        Sprint starts.{' '}
        <span className="text-gradient">Nothing burned yet.</span>
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
          className="slide22-frame"
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
            src={asset('assets/burndown_start.png')}
            alt="Sprint start burndown chart, Monday April 3 2026"
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
          className="slide22-caption"
          style={{
            fontFamily: 'var(--font-body)',
            fontSize: 17,
            color: 'var(--color-text-muted)',
            letterSpacing: '0.02em',
            textAlign: 'center',
            maxWidth: 1200,
          }}
        >
          Jira burndown, captured at the start of the Epic #9 sprint — ~26 story points queued.
        </div>
      </div>
    </SlideFrame>
  );
}

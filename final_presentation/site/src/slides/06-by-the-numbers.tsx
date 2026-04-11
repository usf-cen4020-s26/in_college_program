import { useEffect, useState } from 'react';
import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';

const STATS = [
  { to: 9, label: 'Epics', suffix: '' },
  { to: 28, label: 'Source Files', suffix: '' },
  { to: 4350, label: 'Lines of COBOL', suffix: '+' },
  { to: 195, label: 'Test Fixtures', suffix: '' },
  { to: 7, label: '.DAT Files', suffix: '' },
] as const;

/**
 * Slide 6 — By the Numbers. Five animated stat counters using
 * a lightweight CSS/JS counter instead of heavy Remotion Player.
 */
export function Slide06Numbers(_props: SlideProps) {
  return (
    <SlideFrame act="PART A · AGILE AT A GLANCE" variant="hero" showWordmark>
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(36px, 4.5em, 72px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
          textAlign: 'center',
          flexShrink: 0,
        }}
      >
        By the <span className="text-gradient">Numbers.</span>
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
        <div
          style={{
            display: 'flex',
            gap: 'clamp(8px, 1em, 20px)',
            flexWrap: 'wrap',
            justifyContent: 'center',
            alignItems: 'stretch',
          }}
        >
          {STATS.map((stat, i) => (
            <div
              key={stat.label}
              style={{
                display: 'flex',
                flexDirection: 'column',
                alignItems: 'center',
                justifyContent: 'center',
                gap: 6,
                padding: 'clamp(12px, 1.25em, 24px) clamp(16px, 1.5em, 28px)',
                borderRadius: 14,
                background: 'var(--color-bg-card)',
                border: '1px solid rgba(133,197,255,0.1)',
                minWidth: 'clamp(90px, 8em, 150px)',
                animation: `s06-rise 800ms cubic-bezier(0.16,1,0.3,1) both ${200 + i * 100}ms`,
              }}
            >
              <AnimatedNumber to={stat.to} suffix={stat.suffix} delay={300 + i * 100} />
              <span
                style={{
                  fontFamily: 'var(--font-body)',
                  fontSize: 'clamp(10px, 0.8em, 13px)',
                  fontWeight: 600,
                  letterSpacing: '0.12em',
                  textTransform: 'uppercase',
                  color: 'var(--color-text-dim)',
                  textAlign: 'center',
                }}
              >
                {stat.label}
              </span>
            </div>
          ))}
        </div>
      </div>

      <style>{`
        @keyframes s06-rise {
          0% { opacity: 0; transform: translateY(14px); }
          100% { opacity: 1; transform: translateY(0); }
        }
      `}</style>
    </SlideFrame>
  );
}

function AnimatedNumber({ to, suffix, delay }: { to: number; suffix: string; delay: number }) {
  const [value, setValue] = useState(0);

  useEffect(() => {
    const start = performance.now();
    const duration = 1200;
    let raf: number;

    const tick = (now: number) => {
      const elapsed = now - start - delay;
      if (elapsed < 0) {
        raf = requestAnimationFrame(tick);
        return;
      }
      const t = Math.min(elapsed / duration, 1);
      const eased = 1 - Math.pow(1 - t, 3); // easeOutCubic
      setValue(Math.round(eased * to));
      if (t < 1) raf = requestAnimationFrame(tick);
    };

    raf = requestAnimationFrame(tick);
    return () => cancelAnimationFrame(raf);
  }, [to, delay]);

  return (
    <div
      className="text-gradient"
      style={{
        fontFamily: 'var(--font-mono)',
        fontWeight: 700,
        fontSize: 'clamp(36px, 4.5em, 72px)',
        lineHeight: 1,
        letterSpacing: '-0.02em',
        fontVariantNumeric: 'tabular-nums',
      }}
    >
      {value.toLocaleString('en-US')}{suffix}
    </div>
  );
}

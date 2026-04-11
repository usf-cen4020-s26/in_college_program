import { useEffect, useState } from 'react';
import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';

const STATS = [
  { to: 9, label: 'epics completed', suffix: '', prefix: '' },
  { to: 28, label: 'source files', suffix: '', prefix: '' },
  { to: 4350, label: 'lines of COBOL', suffix: '+', prefix: '' },
  { to: 195, label: 'test fixtures', suffix: '', prefix: '' },
  { to: 7, label: '.DAT data files', suffix: '', prefix: '' },
  { to: 65, label: 'git commits', suffix: '', prefix: '~' },
  { to: 15, label: 'PRs merged', suffix: '+', prefix: '' },
  { to: 11, label: 'weeks · Jan 21 – Apr 8', suffix: '', prefix: '' },
] as const;

/**
 * Slide 60 — Final Statistics with animated counters.
 */
export function Slide60FinalStats(_props: SlideProps) {
  return (
    <SlideFrame act="PART D · ARCHITECTURE" kicker="BY THE NUMBERS" variant="hero">
      <div
        className="stats-grid"
        style={{
          flex: 1,
          display: 'grid',
          gridTemplateColumns: 'repeat(4, 1fr)',
          gap: 'clamp(12px, 1.25em, 22px)',
          alignContent: 'center',
          minHeight: 0,
        }}
      >
        {STATS.map((s, i) => (
          <AnimatedStatCard key={s.label} {...s} delay={200 + i * 80} />
        ))}
      </div>
    </SlideFrame>
  );
}

function AnimatedStatCard({
  to,
  label,
  suffix,
  prefix,
  delay,
}: {
  to: number;
  label: string;
  suffix: string;
  prefix: string;
  delay: number;
}) {
  const [value, setValue] = useState(0);

  useEffect(() => {
    const start = performance.now();
    const duration = 1400;
    let raf: number;
    const tick = (now: number) => {
      const elapsed = now - start - delay;
      if (elapsed < 0) { raf = requestAnimationFrame(tick); return; }
      const t = Math.min(elapsed / duration, 1);
      const eased = 1 - Math.pow(1 - t, 3);
      setValue(Math.round(eased * to));
      if (t < 1) raf = requestAnimationFrame(tick);
    };
    raf = requestAnimationFrame(tick);
    return () => cancelAnimationFrame(raf);
  }, [to, delay]);

  return (
    <div
      style={{
        padding: 'clamp(14px, 1.5em, 24px) clamp(16px, 1.75em, 28px)',
        borderRadius: 14,
        background: 'var(--color-bg-panel)',
        border: '1px solid rgba(133,197,255,0.12)',
        display: 'flex',
        flexDirection: 'column',
        justifyContent: 'center',
        gap: 4,
        minHeight: 0,
        overflow: 'hidden',
        animation: `s60-rise 600ms cubic-bezier(0.16,1,0.3,1) both ${delay}ms`,
      }}
    >
      <div
        className="text-gradient"
        style={{
          fontFamily: 'var(--font-mono)',
          fontWeight: 700,
          fontSize: 'clamp(36px, 4.5em, 64px)',
          lineHeight: 1,
          letterSpacing: '-0.02em',
          fontVariantNumeric: 'tabular-nums',
        }}
      >
        {prefix}{value.toLocaleString('en-US')}{suffix}
      </div>
      <div
        style={{
          fontFamily: 'var(--font-display)',
          fontWeight: 600,
          fontSize: 'clamp(11px, 1em, 16px)',
          textTransform: 'uppercase',
          letterSpacing: '0.16em',
          color: 'var(--color-text-muted)',
        }}
      >
        {label}
      </div>
      <style>{`
        @keyframes s60-rise {
          0% { opacity: 0; transform: translateY(12px) scale(0.96); }
          100% { opacity: 1; transform: translateY(0) scale(1); }
        }
      `}</style>
    </div>
  );
}

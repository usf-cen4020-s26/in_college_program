import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';

const conventions: { range: string; label: string }[] = [
  { range: '1xxx', label: 'Initialization & data loading' },
  { range: '3xxx', label: 'Authentication' },
  { range: '4xxx', label: 'Account creation' },
  { range: '5xxx', label: 'Job features' },
  { range: '6xxx', label: 'Skills menu' },
  { range: '7xxx', label: 'User features (profiles, connections, messaging)' },
  { range: '8xxx', label: 'I/O utilities' },
  { range: '9xxx', label: 'Termination & file writes' },
];

/**
 * Slide 44 — Paragraph Numbering Convention.
 */
export function Slide44Paragraphs(_props: SlideProps) {
  return (
    <SlideFrame act="INTERLUDE" kicker="MODULARIZATION · CONVENTIONS">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(36px, 4em, 64px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
          lineHeight: 1.05,
        }}
      >
        Paragraph <span className="text-gradient">Numbering</span>
      </h1>

      <div
        style={{
          display: 'flex',
          flexDirection: 'column',
          gap: 'clamp(6px, 0.6em, 12px)',
          flex: 1,
          justifyContent: 'center',
        }}
      >
        {conventions.map((c) => (
          <div
            key={c.range}
            style={{
              display: 'flex',
              alignItems: 'center',
              gap: 'clamp(16px, 1.5em, 28px)',
              padding: 'clamp(12px, 1em, 20px) clamp(16px, 1.5em, 28px)',
              borderRadius: 14,
              background: 'var(--color-bg-card)',
              border: '1px solid rgba(112,181,249,0.08)',
            }}
          >
            <span
              style={{
                fontFamily: 'var(--font-mono)',
                fontSize: 'clamp(20px, 2em, 34px)',
                fontWeight: 700,
                color: 'var(--color-brand-accent)',
                minWidth: 'clamp(80px, 6em, 120px)',
                letterSpacing: '-0.01em',
              }}
            >
              {c.range}
            </span>
            <span
              style={{
                fontFamily: 'var(--font-body)',
                fontSize: 'clamp(15px, 1.15em, 20px)',
                color: 'var(--color-text-muted)',
                lineHeight: 1.4,
              }}
            >
              {c.label}
            </span>
          </div>
        ))}
      </div>
    </SlideFrame>
  );
}

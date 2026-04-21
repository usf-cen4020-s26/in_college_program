import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StatCard } from '../components/StatCard';

const modules = [
  { name: 'runner.py', lines: 255 },
  { name: 'persistence.py', lines: 406 },
  { name: 'models.py', lines: 137 },
  { name: 'discovery.py', lines: 161 },
  { name: 'cli.py', lines: 347 },
  { name: 'live_cli.py', lines: 323 },
  { name: 'preprocessing.py', lines: 320 },
  { name: 'packaging.py', lines: 554 },
  { name: 'macros.py', lines: 145 },
  { name: 'constants.py', lines: 101 },
  { name: 'reporting.py', lines: 143 },
];

/**
 * Slide 37 — Test Runner Becomes a Package.
 */
export function Slide37M6Package(_props: SlideProps) {
  return (
    <SlideFrame act="PART B · BUILDING INCOLLEGE" kicker="EPIC #6 · INFRASTRUCTURE">
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
        A Real Test <span className="text-gradient">Package</span>
      </h1>

      <div
        style={{
          display: 'grid',
          gridTemplateColumns: '1.4fr 1fr',
          gap: 48,
          flex: 1,
          minHeight: 0,
          alignItems: 'center',
        }}
      >
        {/* Module tree */}
        <div
          style={{
            borderRadius: 20,
            background: 'var(--color-bg-card)',
            border: '1px solid rgba(112,181,249,0.1)',
            padding: 'clamp(20px, 2em, 36px)',
            fontFamily: 'var(--font-mono)',
            fontSize: 'clamp(13px, 1em, 16px)',
            lineHeight: 1.8,
            color: 'var(--color-text-primary)',
          }}
        >
          <div style={{ color: 'var(--color-brand-accent)', marginBottom: 8 }}>
            tests/incollege_tests/
          </div>
          {modules.map((mod, i) => {
            const isLast = i === modules.length - 1;
            const prefix = isLast ? '  \u2514\u2500\u2500 ' : '  \u251C\u2500\u2500 ';
            return (
              <div key={mod.name} style={{ display: 'flex', justifyContent: 'space-between' }}>
                <span>
                  {prefix}
                  {mod.name}
                </span>
                <span style={{ color: 'var(--color-text-dim)' }}>
                  ({mod.lines} lines)
                </span>
              </div>
            );
          })}
        </div>

        {/* Stat */}
        <div style={{ display: 'flex', justifyContent: 'center' }}>
          <StatCard value="~130" label="Tests" sublabel="running total across all epics" />
        </div>
      </div>
    </SlideFrame>
  );
}

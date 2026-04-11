import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StatCard } from '../components/StatCard';
import { FeatureCallout } from '../components/FeatureCallout';

/**
 * Slide 28 — Testing at Scale.
 * Two-column: left stat, right FeatureCallout for multi-part tests.
 */
export function Slide28M5Testing(_props: SlideProps) {
  return (
    <SlideFrame act="PART B · BUILDING INCOLLEGE" kicker="EPIC #5 · TESTING">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(48px, 5.5em, 88px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
          lineHeight: 1.05,
        }}
      >
        Testing at Scale
      </h1>

      <div
        style={{
          display: 'grid',
          gridTemplateColumns: '1fr 1fr',
          gap: 48,
          flex: 1,
          minHeight: 0,
          alignItems: 'center',
        }}
      >
        {/* Left: stats */}
        <div style={{ display: 'flex', flexDirection: 'column', gap: 24 }}>
          <StatCard
            value="110+"
            label="test fixtures"
            sublabel="80+ new in this epic"
          />
        </div>

        {/* Right: multi-part tests explanation */}
        <div style={{ display: 'flex', flexDirection: 'column', gap: 20 }}>
          <FeatureCallout
            title="Multi-part tests"
            detail="Part 1 sends a connection request, Part 2 verifies it persists across restart — proving the DAT files work end-to-end."
          />
          <div
            style={{
              padding: '20px 24px',
              borderRadius: 16,
              background: 'var(--color-bg-panel)',
              border: '1px solid rgba(112,181,249,0.12)',
              boxShadow:
                '0 20px 40px -24px rgba(0,0,0,0.75), inset 0 1px 0 rgba(255,255,255,0.04)',
            }}
          >
            <div
              style={{
                fontFamily: 'var(--font-mono)',
                fontSize: 'clamp(12px, 0.9em, 15px)',
                color: 'var(--color-brand-accent)',
                fontWeight: 600,
                marginBottom: 8,
              }}
            >
              TEST_RUNNER_GUIDE.md
            </div>
            <p
              style={{
                margin: 0,
                fontFamily: 'var(--font-body)',
                fontSize: 'clamp(13px, 1em, 16px)',
                lineHeight: 1.5,
                color: 'var(--color-text-muted)',
              }}
            >
              226 lines of team documentation covering fixture format, comment
              syntax, multi-part tests, and debug mode.
            </p>
          </div>
        </div>
      </div>
    </SlideFrame>
  );
}

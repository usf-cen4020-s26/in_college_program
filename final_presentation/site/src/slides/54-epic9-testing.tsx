import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StatCard } from '../components/StatCard';

const TEST_CATEGORIES = [
  'Empty inbox',
  'Single/multiple messages',
  'Recipient isolation',
  'Persistence across restarts',
  'Send and view in same session',
  'Message ordering',
];

/**
 * Slide 54 — Epic 9 Testing.
 */
export function Slide54Testing(_props: SlideProps) {
  return (
    <SlideFrame act="PART C · EPIC 9 DEEP DIVE" kicker="TESTING">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(36px, 4em, 64px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          lineHeight: 1.05,
        }}
      >
        <span className="text-gradient">60+ New Fixtures</span>
      </h1>

      <div
        style={{
          flex: 1,
          display: 'grid',
          gridTemplateColumns: '1.2fr 1fr',
          gap: 48,
          alignItems: 'center',
        }}
      >
        {/* Test categories */}
        <div
          style={{
            display: 'flex',
            flexDirection: 'column',
            gap: 16,
          }}
        >
          {TEST_CATEGORIES.map((cat) => (
            <div
              key={cat}
              style={{
                padding: '14px 22px',
                borderRadius: 12,
                background: 'var(--color-bg-panel)',
                border: '1px solid rgba(112,181,249,0.12)',
                fontFamily: 'var(--font-body)',
                fontSize: 'clamp(15px, 1.2em, 20px)',
                color: 'var(--color-text-primary)',
                display: 'flex',
                alignItems: 'center',
                gap: 14,
                boxShadow: '0 8px 24px -12px rgba(0,0,0,0.5)',
              }}
            >
              <span style={{ color: 'var(--color-brand-accent)' }}>&#10003;</span>
              {cat}
            </div>
          ))}
        </div>

        {/* Stat */}
        <div style={{ display: 'flex', justifyContent: 'center' }}>
          <StatCard
            value="195"
            label="total test fixtures"
            sublabel="across all 9 epics"
          />
        </div>
      </div>
    </SlideFrame>
  );
}

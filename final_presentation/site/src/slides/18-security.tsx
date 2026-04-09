import type { CSSProperties } from 'react';
import { SlideFrame } from '../components/SlideFrame';
import { SECURITY_FEATURES } from '../data/security';
import type { SecurityFeature } from '../data/security';
import type { SlideProps } from './types';

const ICON_GLYPH: Record<string, string> = {
  shield: '\u{1F6E1}',
  lock: '\u{1F512}',
  'eye-off': '\u{1F441}',
  inbox: '\u{1F4E5}',
  link: '\u{1F517}',
  check: '\u2713',
};

export function Slide18Security(_props: SlideProps) {
  return (
    <SlideFrame act="ACT III · HOW WE BUILT IT" kicker="SECURITY">
      <div>
        <h1
          style={{
            margin: 0,
            fontFamily: 'var(--font-display)',
            fontWeight: 700,
            fontSize: 76,
            lineHeight: 1.05,
            letterSpacing: '-0.02em',
            color: 'var(--color-text-primary)',
          }}
        >
          Six rails around the product.
        </h1>
      </div>

      <div
        style={{
          display: 'grid',
          gridTemplateColumns: 'repeat(3, 1fr)',
          gridTemplateRows: 'repeat(2, 1fr)',
          gap: 26,
          flex: 1,
          minHeight: 0,
        }}
      >
        {SECURITY_FEATURES.map((feature, i) => (
          <SecurityCard key={feature.title} feature={feature} index={i} />
        ))}
      </div>

      <div
        style={{
          fontFamily: 'var(--font-body)',
          fontSize: 14,
          color: 'var(--color-text-dim)',
          letterSpacing: '0.02em',
        }}
      >
        Every claim cites a real paragraph in{' '}
        <code
          style={{
            fontFamily: 'var(--font-mono)',
            fontSize: 13,
            color: 'var(--color-text-muted)',
          }}
        >
          src/*.cpy
        </code>
        . See{' '}
        <code
          style={{
            fontFamily: 'var(--font-mono)',
            fontSize: 13,
            color: 'var(--color-text-muted)',
          }}
        >
          scripts/verify.sh
        </code>
        .
      </div>

      <style>{`
        @keyframes slide18-in {
          from { opacity: 0; transform: translateY(14px); }
          to { opacity: 1; transform: translateY(0); }
        }
      `}</style>
    </SlideFrame>
  );
}

function SecurityCard({
  feature,
  index,
}: {
  feature: SecurityFeature;
  index: number;
}) {
  const glyph = ICON_GLYPH[feature.icon] ?? null;
  const cardStyle: CSSProperties = {
    padding: '26px 28px',
    borderRadius: 20,
    background: 'var(--color-bg-panel)',
    border: '1px solid rgba(112,181,249,0.14)',
    boxShadow:
      '0 28px 56px -30px rgba(0,0,0,0.8), inset 0 1px 0 rgba(255,255,255,0.04)',
    display: 'flex',
    flexDirection: 'column',
    gap: 14,
    opacity: 0,
    animation: `slide18-in 480ms ${160 + index * 80}ms both cubic-bezier(0.16,1,0.3,1)`,
    minWidth: 0,
  };

  return (
    <div style={cardStyle}>
      <div style={{ display: 'flex', alignItems: 'center', gap: 14 }}>
        <div
          style={{
            width: 44,
            height: 44,
            borderRadius: 12,
            background: 'rgba(10,102,194,0.22)',
            border: '1px solid rgba(112,181,249,0.32)',
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
            color: 'var(--color-brand-accent)',
            fontSize: 22,
            flexShrink: 0,
          }}
        >
          {glyph ?? (
            <span
              style={{
                width: 10,
                height: 10,
                borderRadius: '50%',
                background: 'var(--color-brand-accent)',
                display: 'block',
              }}
            />
          )}
        </div>
        <div
          style={{
            fontFamily: 'var(--font-display)',
            fontWeight: 700,
            fontSize: 22,
            letterSpacing: '-0.01em',
            color: 'var(--color-text-primary)',
            lineHeight: 1.2,
          }}
        >
          {feature.title}
        </div>
      </div>
      <p
        style={{
          margin: 0,
          fontFamily: 'var(--font-body)',
          fontSize: 14,
          lineHeight: 1.55,
          color: 'var(--color-text-muted)',
          flex: 1,
        }}
      >
        {feature.detail}
      </p>
      <div
        style={{
          fontFamily: 'var(--font-mono)',
          fontSize: 12,
          color: 'var(--color-text-dim)',
          letterSpacing: '0.01em',
          paddingTop: 6,
          borderTop: '1px solid rgba(112,181,249,0.08)',
        }}
      >
        {feature.citation}
      </div>
    </div>
  );
}

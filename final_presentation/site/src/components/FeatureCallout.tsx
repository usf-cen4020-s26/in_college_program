import type { ReactNode } from 'react';

interface FeatureCalloutProps {
  title: string;
  detail: string;
  citation?: string;
  icon?: ReactNode;
}

/**
 * Labeled feature box used on the security and feature slides. Renders
 * a bg-panel card with an icon + title row, a muted detail paragraph,
 * and a small mono citation (e.g. `src/AUTH.cpy:230`).
 */
export function FeatureCallout({ title, detail, citation, icon }: FeatureCalloutProps) {
  return (
    <div
      style={{
        padding: '26px 28px',
        borderRadius: 18,
        background: 'var(--color-bg-panel)',
        border: '1px solid rgba(112,181,249,0.12)',
        boxShadow:
          '0 24px 48px -28px rgba(0,0,0,0.75), inset 0 1px 0 rgba(255,255,255,0.04)',
        display: 'flex',
        flexDirection: 'column',
        gap: 14,
      }}
    >
      <div style={{ display: 'flex', alignItems: 'center', gap: 14 }}>
        {icon && (
          <div
            style={{
              width: 36,
              height: 36,
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
              borderRadius: 10,
              background: 'rgba(10,102,194,0.22)',
              border: '1px solid rgba(112,181,249,0.28)',
              color: 'var(--color-brand-accent)',
              flexShrink: 0,
            }}
          >
            {icon}
          </div>
        )}
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
          {title}
        </div>
      </div>

      <p
        style={{
          margin: 0,
          fontFamily: 'var(--font-body)',
          fontSize: 15,
          lineHeight: 1.55,
          color: 'var(--color-text-muted)',
        }}
      >
        {detail}
      </p>

      {citation && (
        <div
          style={{
            fontFamily: 'var(--font-mono)',
            fontSize: 12,
            color: 'var(--color-text-dim)',
            letterSpacing: '0.01em',
          }}
        >
          {citation}
        </div>
      )}
    </div>
  );
}

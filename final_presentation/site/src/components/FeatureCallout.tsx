import type { ReactNode } from 'react';

interface FeatureCalloutProps {
  title: string;
  detail: string;
  citation?: string;
  icon?: ReactNode;
}

/**
 * Labeled feature box used on the security and feature slides.
 */
export function FeatureCallout({ title, detail, citation, icon }: FeatureCalloutProps) {
  return (
    <div
      style={{
        padding: 'clamp(16px, 1.5em, 28px) clamp(18px, 1.75em, 32px)',
        borderRadius: 14,
        background: 'var(--color-bg-panel)',
        border: '1px solid rgba(133,197,255,0.12)',
        boxShadow:
          '0 24px 48px -28px rgba(0,0,0,0.75), inset 0 1px 0 rgba(255,255,255,0.04)',
        display: 'flex',
        flexDirection: 'column',
        gap: 'clamp(6px, 0.5em, 10px)',
      }}
    >
      <div style={{ display: 'flex', alignItems: 'center', gap: 'clamp(8px, 0.75em, 12px)' }}>
        {icon && (
          <div
            style={{
              width: 'clamp(28px, 2.5em, 40px)',
              height: 'clamp(28px, 2.5em, 40px)',
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
              borderRadius: 8,
              background: 'rgba(10,102,194,0.22)',
              border: '1px solid rgba(133,197,255,0.28)',
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
            fontSize: 'clamp(18px, 1.5em, 26px)',
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
          fontSize: 'clamp(14px, 1.1em, 19px)',
          lineHeight: 1.5,
          color: 'var(--color-text-muted)',
        }}
      >
        {detail}
      </p>

      {citation && (
        <div
          style={{
            fontFamily: 'var(--font-mono)',
            fontSize: 'clamp(11px, 0.85em, 14px)',
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

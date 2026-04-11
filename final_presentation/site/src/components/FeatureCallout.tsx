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
        padding: 'clamp(12px, 1.25em, 22px) clamp(14px, 1.5em, 24px)',
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
              width: 'clamp(24px, 2em, 32px)',
              height: 'clamp(24px, 2em, 32px)',
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
            fontSize: 'clamp(14px, 1.25em, 20px)',
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
          fontSize: 'clamp(11px, 0.9em, 15px)',
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
            fontSize: 'clamp(8px, 0.625em, 10px)',
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

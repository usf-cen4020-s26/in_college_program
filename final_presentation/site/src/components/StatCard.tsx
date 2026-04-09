import clsx from 'clsx';

interface StatCardProps {
  value: string;
  label: string;
  sublabel?: string;
  tone?: 'brand' | 'success' | 'warn';
}

/**
 * Big-number stat card. Value renders in Plex Mono 700 at ~88px with
 * tabular figures. `tone` swaps the color: brand (default) uses the
 * headline gradient via the `.text-gradient` class; success/warn swap
 * in flat palette colors.
 */
export function StatCard({ value, label, sublabel, tone = 'brand' }: StatCardProps) {
  const valueColor =
    tone === 'success'
      ? 'var(--color-state-success)'
      : tone === 'warn'
        ? 'var(--color-state-warn)'
        : undefined;

  return (
    <div
      style={{
        padding: '36px 40px 34px',
        borderRadius: 20,
        background: 'var(--color-bg-panel)',
        border: '1px solid rgba(112,181,249,0.12)',
        boxShadow:
          '0 30px 60px -32px rgba(0,0,0,0.8), inset 0 1px 0 rgba(255,255,255,0.04)',
        display: 'flex',
        flexDirection: 'column',
        gap: 10,
        minWidth: 260,
      }}
    >
      <div
        className={clsx(tone === 'brand' && 'text-gradient')}
        style={{
          fontFamily: 'var(--font-mono)',
          fontWeight: 700,
          fontSize: 88,
          lineHeight: 1,
          letterSpacing: '-0.02em',
          fontVariantNumeric: 'tabular-nums',
          color: valueColor,
        }}
      >
        {value}
      </div>
      <div
        style={{
          fontFamily: 'var(--font-display)',
          fontWeight: 600,
          fontSize: 13,
          textTransform: 'uppercase',
          letterSpacing: '0.2em',
          color: 'var(--color-text-primary)',
        }}
      >
        {label}
      </div>
      {sublabel && (
        <div
          style={{
            fontFamily: 'var(--font-body)',
            fontSize: 14,
            color: 'var(--color-text-muted)',
            lineHeight: 1.4,
          }}
        >
          {sublabel}
        </div>
      )}
    </div>
  );
}

interface PersonCardProps {
  name: string;
  role: string;
  initials: string;
  accent?: string;
}

/**
 * Team-member card. A ~320px wide panel with a gradient monogram circle,
 * name in Plex Sans 700, and a small uppercase-tracked role chip.
 *
 * `accent` overrides the monogram gradient's ending hue for a bit of
 * per-person variation (defaults to brand accent blue).
 */
export function PersonCard({ name, role, initials, accent = '#70B5F9' }: PersonCardProps) {
  return (
    <div
      style={{
        width: 320,
        padding: '28px 28px 30px',
        borderRadius: 20,
        background: 'var(--color-bg-panel)',
        border: '1px solid rgba(112,181,249,0.14)',
        boxShadow:
          '0 30px 60px -32px rgba(0,0,0,0.8), inset 0 1px 0 rgba(255,255,255,0.04)',
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'flex-start',
        gap: 18,
      }}
    >
      <div
        style={{
          width: 64,
          height: 64,
          borderRadius: '50%',
          background: `linear-gradient(135deg, #0A66C2 0%, ${accent} 100%)`,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          color: '#F5F7FA',
          fontFamily: 'var(--font-display)',
          fontWeight: 700,
          fontSize: 24,
          letterSpacing: '-0.01em',
          boxShadow: '0 14px 28px -14px rgba(10,102,194,0.75)',
        }}
      >
        {initials}
      </div>

      <div
        style={{
          fontFamily: 'var(--font-display)',
          fontWeight: 700,
          fontSize: 26,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
          lineHeight: 1.15,
        }}
      >
        {name}
      </div>

      <div
        style={{
          display: 'inline-flex',
          padding: '6px 12px',
          borderRadius: 999,
          background: 'rgba(10,102,194,0.22)',
          border: '1px solid rgba(112,181,249,0.28)',
          color: 'var(--color-brand-accent)',
          fontFamily: 'var(--font-body)',
          fontSize: 12,
          fontWeight: 600,
          textTransform: 'uppercase',
          letterSpacing: '0.14em',
        }}
      >
        {role}
      </div>
    </div>
  );
}

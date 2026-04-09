interface WordmarkProps {
  size?: number;
}

/**
 * Animated "InCollege" wordmark. The "in" sits inside a rounded
 * brand-blue pill; "College" trails in Plex Sans 700 white. The `size`
 * prop sets the cap-height in pixels (defaults to 28).
 */
export function Wordmark({ size = 28 }: WordmarkProps) {
  const pillHeight = Math.round(size * 1.25);
  const pillPaddingX = Math.round(size * 0.4);
  const gap = Math.round(size * 0.24);

  return (
    <div
      style={{
        display: 'inline-flex',
        alignItems: 'center',
        gap,
        fontFamily: 'var(--font-display)',
        fontWeight: 700,
        letterSpacing: '-0.02em',
        fontSize: size,
        lineHeight: 1,
      }}
    >
      <span
        style={{
          display: 'inline-flex',
          alignItems: 'center',
          justifyContent: 'center',
          height: pillHeight,
          padding: `0 ${pillPaddingX}px`,
          borderRadius: pillHeight / 2,
          background: 'linear-gradient(135deg, #0A66C2 0%, #38BDF8 100%)',
          color: '#F5F7FA',
          boxShadow: '0 6px 18px -8px rgba(10,102,194,0.65)',
        }}
      >
        in
      </span>
      <span style={{ color: 'var(--color-text-primary)' }}>College</span>
    </div>
  );
}

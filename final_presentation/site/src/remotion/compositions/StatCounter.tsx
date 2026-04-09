import { AbsoluteFill, Easing, interpolate, useCurrentFrame } from 'remotion';
import { palette, typography } from '../../theme';

export type StatCounterProps = {
  to: number;
  prefix?: string;
  suffix?: string;
  durationInFrames?: number;
  label?: string;
  sublabel?: string;
};

/**
 * Count-up stat animation.
 *
 * The number interpolates from 0 to `to` with an easeOutCubic curve and
 * rounds to an integer on each frame so the readout never flashes decimals.
 * The numeral itself uses the brand gradient as a clipped text fill.
 */
export function StatCounter({
  to,
  prefix = '',
  suffix = '',
  durationInFrames = 40,
  label,
  sublabel,
}: StatCounterProps) {
  const frame = useCurrentFrame();

  const raw = interpolate(frame, [0, durationInFrames], [0, to], {
    extrapolateLeft: 'clamp',
    extrapolateRight: 'clamp',
    easing: Easing.out(Easing.cubic),
  });
  const value = Math.round(raw);

  return (
    <AbsoluteFill
      style={{
        backgroundColor: 'transparent',
        alignItems: 'center',
        justifyContent: 'center',
        fontFamily: typography.body,
      }}
    >
      <div
        style={{
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'center',
          gap: 24,
        }}
      >
        <div
          style={{
            fontFamily: typography.stat,
            fontSize: 240,
            fontWeight: 700,
            lineHeight: 1,
            letterSpacing: '-0.03em',
            fontVariantNumeric: 'tabular-nums',
            backgroundImage: palette.grad.headline,
            backgroundClip: 'text',
            WebkitBackgroundClip: 'text',
            WebkitTextFillColor: 'transparent',
            color: palette.brand.accent,
            textShadow: `0 0 80px ${palette.brand.primary}44`,
          }}
        >
          {prefix}
          {value.toLocaleString('en-US')}
          {suffix}
        </div>
        {label && (
          <div
            style={{
              fontFamily: typography.display,
              fontWeight: 700,
              fontSize: 28,
              textTransform: 'uppercase',
              letterSpacing: '0.22em',
              color: palette.text.primary,
            }}
          >
            {label}
          </div>
        )}
        {sublabel && (
          <div
            style={{
              fontFamily: typography.body,
              fontWeight: 500,
              fontSize: 20,
              color: palette.text.muted,
              letterSpacing: '0.02em',
            }}
          >
            {sublabel}
          </div>
        )}
      </div>
    </AbsoluteFill>
  );
}

import { AbsoluteFill, Easing, interpolate, useCurrentFrame } from 'remotion';
import { palette, typography } from '../../theme';

export type CodeRevealProps = {
  /** Lines of code, pre-split. */
  lines: string[];
  /** Syntax-highlighted HTML for each line, from Shiki. Can be null; fallback to raw text. */
  htmlLines?: (string | null)[];
  /** Frames before the first line appears. */
  startDelay?: number;
  /** Frames between successive line appearances. */
  perLine?: number;
};

const ENTRY_DURATION_FRAMES = 8;

/**
 * Line-by-line code reveal with fake terminal chrome and a gutter for
 * line numbers. Each line fades + slides in on its own schedule so the
 * audience gets a moment to read.
 *
 * When `htmlLines[i]` is provided it is injected verbatim — Shiki produces
 * trusted, escaped HTML at build time, so the injection is safe.
 */
export function CodeReveal({
  lines,
  htmlLines,
  startDelay = 4,
  perLine = 5,
}: CodeRevealProps) {
  const frame = useCurrentFrame();

  return (
    <AbsoluteFill
      style={{
        backgroundColor: 'transparent',
        alignItems: 'center',
        justifyContent: 'center',
        padding: 72,
        fontFamily: typography.mono,
      }}
    >
      <div
        style={{
          width: '100%',
          maxWidth: 1600,
          backgroundColor: palette.bg.code,
          border: `1px solid ${palette.bg.codeBorder}`,
          borderRadius: 18,
          overflow: 'hidden',
          boxShadow:
            '0 40px 120px rgba(0,0,0,0.55), inset 0 1px 0 rgba(255,255,255,0.04)',
        }}
      >
        <div
          style={{
            display: 'flex',
            alignItems: 'center',
            gap: 10,
            height: 44,
            padding: '0 18px',
            backgroundColor: '#0A1020',
            borderBottom: `1px solid ${palette.bg.codeBorder}`,
          }}
        >
          <Dot color="#FF5F57" />
          <Dot color="#FEBC2E" />
          <Dot color="#28C840" />
          <div
            style={{
              flex: 1,
              textAlign: 'center',
              color: palette.text.muted,
              fontFamily: typography.body,
              fontSize: 13,
              letterSpacing: '0.04em',
              transform: 'translateX(-30px)',
            }}
          >
            code
          </div>
        </div>
        <div
          style={{
            padding: '28px 0 32px 0',
            fontFamily: typography.mono,
            fontSize: 24,
            lineHeight: 1.55,
          }}
        >
          {lines.map((rawLine, idx) => {
            const appearAt = startDelay + idx * perLine;
            const progress = interpolate(
              frame,
              [appearAt, appearAt + ENTRY_DURATION_FRAMES],
              [0, 1],
              {
                extrapolateLeft: 'clamp',
                extrapolateRight: 'clamp',
                easing: Easing.out(Easing.cubic),
              },
            );
            const translateX = (1 - progress) * -12;
            const highlighted = htmlLines?.[idx] ?? null;

            return (
              <div
                key={idx}
                style={{
                  display: 'flex',
                  alignItems: 'baseline',
                  opacity: progress,
                  transform: `translateX(${translateX}px)`,
                  padding: '0 32px',
                }}
              >
                <span
                  style={{
                    display: 'inline-block',
                    width: 56,
                    textAlign: 'right',
                    marginRight: 24,
                    color: palette.text.dim,
                    fontVariantNumeric: 'tabular-nums',
                    userSelect: 'none',
                  }}
                >
                  {idx + 1}
                </span>
                {highlighted ? (
                  <span
                    style={{
                      flex: 1,
                      whiteSpace: 'pre',
                      color: palette.text.primary,
                    }}
                    // Shiki produces trusted, escaped HTML at build time.
                    // eslint-disable-next-line react/no-danger
                    dangerouslySetInnerHTML={{ __html: highlighted }}
                  />
                ) : (
                  <span
                    style={{
                      flex: 1,
                      whiteSpace: 'pre',
                      color: palette.text.primary,
                    }}
                  >
                    {rawLine.length === 0 ? ' ' : rawLine}
                  </span>
                )}
              </div>
            );
          })}
        </div>
      </div>
    </AbsoluteFill>
  );
}

function Dot({ color }: { color: string }) {
  return (
    <div
      style={{
        width: 13,
        height: 13,
        borderRadius: '50%',
        backgroundColor: color,
      }}
    />
  );
}

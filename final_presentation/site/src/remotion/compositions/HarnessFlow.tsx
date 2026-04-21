import { AbsoluteFill, Easing, interpolate, useCurrentFrame } from 'remotion';
import { palette, typography } from '../../theme';

export type HarnessStep = {
  label: string;
  sublabel?: string;
  icon?: string;
};

export type HarnessFlowProps = {
  /** Step boxes, shown left-to-right. */
  steps: HarnessStep[];
};

const BOX_WIDTH = 260;
const BOX_HEIGHT = 150;
const BOX_GAP = 64;
const STEP_STAGGER = 18;
const BOX_FADE_FRAMES = 12;
const ARROW_DRAW_FRAMES = 10;

/**
 * Horizontal test-harness flow diagram.
 *
 * Boxes fade in left-to-right. Each connecting arrow draws itself with a
 * stroke-dasharray animation as the next box appears. The final step shows
 * a green check for pass.
 */
export function HarnessFlow({ steps }: HarnessFlowProps) {
  const frame = useCurrentFrame();

  const stepCount = steps.length;
  const rowWidth =
    stepCount * BOX_WIDTH + Math.max(0, stepCount - 1) * BOX_GAP;

  // Center the row inside a 1920x1080 canvas.
  const rowX = (1920 - rowWidth) / 2;
  const rowY = (1080 - BOX_HEIGHT) / 2;

  return (
    <AbsoluteFill
      style={{
        backgroundColor: 'transparent',
        fontFamily: typography.body,
      }}
    >
      <svg
        viewBox="0 0 1920 1080"
        width="100%"
        height="100%"
        style={{ display: 'block' }}
      >
        <defs>
          <marker
            id="harness-arrow-head"
            viewBox="0 0 10 10"
            refX="9"
            refY="5"
            markerWidth="7"
            markerHeight="7"
            orient="auto-start-reverse"
          >
            <path d="M 0 0 L 10 5 L 0 10 z" fill={palette.brand.accent} />
          </marker>
        </defs>

        {/* Connecting arrows (drawn first so boxes overlap cleanly). */}
        {steps.slice(0, -1).map((_, idx) => {
          const x1 = rowX + (idx + 1) * BOX_WIDTH + idx * BOX_GAP;
          const x2 = x1 + BOX_GAP;
          const y = rowY + BOX_HEIGHT / 2;

          const startFrame = (idx + 1) * STEP_STAGGER - ARROW_DRAW_FRAMES;
          const drawProgress = interpolate(
            frame,
            [startFrame, startFrame + ARROW_DRAW_FRAMES],
            [0, 1],
            {
              extrapolateLeft: 'clamp',
              extrapolateRight: 'clamp',
              easing: Easing.out(Easing.cubic),
            },
          );
          const currentX2 = x1 + (x2 - x1) * drawProgress;

          return (
            <line
              key={`arrow-${idx}`}
              x1={x1}
              y1={y}
              x2={currentX2}
              y2={y}
              stroke={palette.brand.accent}
              strokeWidth={3}
              markerEnd={
                drawProgress > 0.95 ? 'url(#harness-arrow-head)' : undefined
              }
              opacity={drawProgress > 0 ? 1 : 0}
            />
          );
        })}

        {/* Step boxes. */}
        {steps.map((step, idx) => {
          const startFrame = idx * STEP_STAGGER;
          const progress = interpolate(
            frame,
            [startFrame, startFrame + BOX_FADE_FRAMES],
            [0, 1],
            {
              extrapolateLeft: 'clamp',
              extrapolateRight: 'clamp',
              easing: Easing.out(Easing.cubic),
            },
          );
          const x = rowX + idx * (BOX_WIDTH + BOX_GAP);
          const y = rowY + (1 - progress) * 8;
          const isFinal = idx === steps.length - 1;

          return (
            <g key={`box-${idx}`} opacity={progress}>
              <rect
                x={x}
                y={y}
                width={BOX_WIDTH}
                height={BOX_HEIGHT}
                rx={16}
                ry={16}
                fill={palette.bg.elevated}
                stroke={
                  isFinal ? palette.state.success : palette.brand.accent
                }
                strokeWidth={2}
              />
              {step.icon && (
                <text
                  x={x + BOX_WIDTH / 2}
                  y={y + 42}
                  textAnchor="middle"
                  dominantBaseline="central"
                  fontSize={32}
                >
                  {step.icon}
                </text>
              )}
              {isFinal && !step.icon && (
                <g
                  transform={`translate(${x + BOX_WIDTH / 2 - 18}, ${y + 26})`}
                >
                  <circle
                    cx={18}
                    cy={18}
                    r={16}
                    fill="none"
                    stroke={palette.state.success}
                    strokeWidth={3}
                  />
                  <path
                    d="M 10 19 L 16 25 L 27 12"
                    fill="none"
                    stroke={palette.state.success}
                    strokeWidth={3}
                    strokeLinecap="round"
                    strokeLinejoin="round"
                  />
                </g>
              )}
              <text
                x={x + BOX_WIDTH / 2}
                y={y + (step.icon || isFinal ? 92 : 70)}
                textAnchor="middle"
                dominantBaseline="central"
                fill={palette.text.primary}
                fontFamily={typography.display}
                fontSize={22}
                fontWeight={700}
                letterSpacing="1.5"
                style={{ textTransform: 'uppercase' }}
              >
                {step.label}
              </text>
              {step.sublabel && (
                <text
                  x={x + BOX_WIDTH / 2}
                  y={y + (step.icon || isFinal ? 122 : 102)}
                  textAnchor="middle"
                  dominantBaseline="central"
                  fill={palette.text.muted}
                  fontFamily={typography.mono}
                  fontSize={16}
                >
                  {step.sublabel}
                </text>
              )}
            </g>
          );
        })}
      </svg>
    </AbsoluteFill>
  );
}

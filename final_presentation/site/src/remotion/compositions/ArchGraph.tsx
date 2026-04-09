import {
  AbsoluteFill,
  interpolate,
  spring,
  useCurrentFrame,
  useVideoConfig,
} from 'remotion';
import { palette, typography } from '../../theme';

export type ArchGraphCluster = {
  label: string;
  color: string;
  nodes: readonly string[];
};

export type ArchGraphProps = {
  /** Center label — typically 'main.cob'. */
  center: string;
  /** Orbiting copybook node labels, grouped by cluster. */
  clusters: readonly ArchGraphCluster[];
};

const VIEWBOX = { w: 1920, h: 1080 };
const CENTER = { x: VIEWBOX.w / 2, y: VIEWBOX.h / 2 };
const CENTER_RADIUS = 80;
const NODE_RADIUS = 32;
const STAGGER_FRAMES = 6;

/**
 * Constellation layout: main.cob sits at the center, clusters fan out
 * around it, and each cluster's copybooks arc in a ring. Nodes spring
 * out from the center with a staggered delay.
 */
export function ArchGraph({ center, clusters }: ArchGraphProps) {
  const frame = useCurrentFrame();
  const { fps } = useVideoConfig();

  // Divide the full circle into sectors, one per cluster.
  const clusterCount = clusters.length;
  const sectorAngle = (Math.PI * 2) / Math.max(1, clusterCount);

  // Flatten nodes with a deterministic index for staggering.
  let flatIndex = 0;
  const clusterData = clusters.map((cluster, clusterIdx) => {
    // Center of the sector this cluster occupies.
    const sectorCenter = clusterIdx * sectorAngle - Math.PI / 2;
    // Radius of the ring this cluster's nodes sit on.
    const ringRadius = Math.min(VIEWBOX.w, VIEWBOX.h) * 0.33;

    // Spread nodes across the sector. Keep them bunched so clusters
    // remain visually distinct.
    const spread = sectorAngle * 0.7;
    const nodes = cluster.nodes.map((label, nodeIdx) => {
      const t =
        cluster.nodes.length === 1
          ? 0
          : nodeIdx / (cluster.nodes.length - 1) - 0.5;
      const angle = sectorCenter + t * spread;
      const x = CENTER.x + Math.cos(angle) * ringRadius;
      const y = CENTER.y + Math.sin(angle) * ringRadius;
      const delay = flatIndex * STAGGER_FRAMES;
      flatIndex++;
      return { label, x, y, angle, delay };
    });

    // Label sits a little further out than the ring, along the sector center.
    const labelRadius = ringRadius + 140;
    const labelX = CENTER.x + Math.cos(sectorCenter) * labelRadius;
    const labelY = CENTER.y + Math.sin(sectorCenter) * labelRadius;

    return { ...cluster, nodes, labelX, labelY };
  });

  return (
    <AbsoluteFill
      style={{
        backgroundColor: 'transparent',
        fontFamily: typography.body,
      }}
    >
      <svg
        viewBox={`0 0 ${VIEWBOX.w} ${VIEWBOX.h}`}
        width="100%"
        height="100%"
        style={{ display: 'block' }}
      >
        <defs>
          <radialGradient id="arch-center-glow" cx="50%" cy="50%" r="50%">
            <stop offset="0%" stopColor={palette.brand.accent} stopOpacity="0.45" />
            <stop offset="100%" stopColor={palette.brand.primary} stopOpacity="0" />
          </radialGradient>
        </defs>

        {/* Cluster connection lines drawn first so nodes sit on top. */}
        {clusterData.map((cluster) =>
          cluster.nodes.map((node, nodeIdx) => {
            const nodeProgress = spring({
              frame: frame - node.delay,
              fps,
              config: { damping: 14, stiffness: 90, mass: 0.8 },
            });
            const lineOpacity = interpolate(
              nodeProgress,
              [0, 0.3, 1],
              [0, 0.15, 0.55],
              {
                extrapolateLeft: 'clamp',
                extrapolateRight: 'clamp',
              },
            );
            const x2 =
              CENTER.x + (node.x - CENTER.x) * nodeProgress;
            const y2 =
              CENTER.y + (node.y - CENTER.y) * nodeProgress;
            return (
              <line
                key={`${cluster.label}-${nodeIdx}-line`}
                x1={CENTER.x}
                y1={CENTER.y}
                x2={x2}
                y2={y2}
                stroke={cluster.color}
                strokeWidth={2}
                strokeOpacity={lineOpacity}
              />
            );
          }),
        )}

        {/* Center glow halo. */}
        <circle
          cx={CENTER.x}
          cy={CENTER.y}
          r={CENTER_RADIUS * 3}
          fill="url(#arch-center-glow)"
        />

        {/* Center node: main.cob. */}
        <circle
          cx={CENTER.x}
          cy={CENTER.y}
          r={CENTER_RADIUS}
          fill={palette.brand.primary}
          stroke={palette.brand.accent}
          strokeWidth={3}
        />
        <text
          x={CENTER.x}
          y={CENTER.y}
          textAnchor="middle"
          dominantBaseline="central"
          fill={palette.text.primary}
          fontFamily={typography.mono}
          fontSize={24}
          fontWeight={700}
        >
          {center}
        </text>

        {/* Cluster labels. */}
        {clusterData.map((cluster, clusterIdx) => {
          // Fade cluster labels in alongside their first node.
          const firstNodeDelay = cluster.nodes[0]?.delay ?? 0;
          const labelProgress = spring({
            frame: frame - firstNodeDelay,
            fps,
            config: { damping: 18, stiffness: 120, mass: 0.6 },
          });
          return (
            <text
              key={`${cluster.label}-label-${clusterIdx}`}
              x={cluster.labelX}
              y={cluster.labelY}
              textAnchor="middle"
              dominantBaseline="central"
              fill={cluster.color}
              fontFamily={typography.display}
              fontSize={22}
              fontWeight={700}
              letterSpacing="4"
              opacity={labelProgress}
              style={{ textTransform: 'uppercase' }}
            >
              {cluster.label}
            </text>
          );
        })}

        {/* Orbiting nodes. */}
        {clusterData.map((cluster) =>
          cluster.nodes.map((node, nodeIdx) => {
            const nodeProgress = spring({
              frame: frame - node.delay,
              fps,
              config: { damping: 14, stiffness: 90, mass: 0.8 },
            });
            const x =
              CENTER.x + (node.x - CENTER.x) * nodeProgress;
            const y =
              CENTER.y + (node.y - CENTER.y) * nodeProgress;
            return (
              <g
                key={`${cluster.label}-${nodeIdx}-node`}
                opacity={nodeProgress}
              >
                <circle
                  cx={x}
                  cy={y}
                  r={NODE_RADIUS}
                  fill={palette.bg.elevated}
                  stroke={cluster.color}
                  strokeWidth={2}
                />
                <text
                  x={x}
                  y={y + NODE_RADIUS + 22}
                  textAnchor="middle"
                  dominantBaseline="hanging"
                  fill={palette.text.primary}
                  fontFamily={typography.mono}
                  fontSize={16}
                  fontWeight={500}
                >
                  {node.label}
                </text>
              </g>
            );
          }),
        )}
      </svg>
    </AbsoluteFill>
  );
}

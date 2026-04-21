import type { SlideProps } from './types';
import type { ModuleCluster } from '../data/modules';
import { SlideFrame } from '../components/SlideFrame';
import { MODULE_CLUSTERS } from '../data/modules';

type Column = readonly ModuleCluster[];

// Balance the 8 clusters across 3 columns by module count. Totals:
// Core 2, Auth 1, Profiles 3, Connections 5, Jobs 5, Messaging 2,
// WS 6, I/O 1 → 25 total ≈ 8.3 per column.
const byName = (name: string): ModuleCluster => {
  const cluster = MODULE_CLUSTERS.find((c) => c.name === name);
  if (!cluster) throw new Error(`Missing cluster: ${name}`);
  return cluster;
};

const COLUMNS: readonly Column[] = [
  [byName('Connections'), byName('Authentication'), byName('Messaging')],
  [byName('Jobs'), byName('Core'), byName('I/O Control')],
  [byName('Working Storage'), byName('Profiles & Discovery')],
];

/**
 * Slide 14 — Trevor — "What each copybook owns".
 *
 * Three balanced columns of cluster sections. Each cluster header is
 * tinted with its ArchGraph color; every module row lists the file name
 * in mono on the left and its one-line responsibility on the right.
 */
export function Slide14CopybookDeepDive(_props: SlideProps) {
  return (
    <SlideFrame act="ACT III · HOW WE BUILT IT" kicker="MODULARITY">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 76,
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
          lineHeight: 1.02,
        }}
      >
        What each <span className="text-gradient">copybook owns.</span>
      </h1>

      <div
        style={{
          display: 'grid',
          gridTemplateColumns: 'repeat(3, 1fr)',
          gap: 40,
          flex: 1,
          minHeight: 0,
        }}
      >
        {COLUMNS.map((column, columnIdx) => (
          <div
            key={columnIdx}
            style={{
              display: 'flex',
              flexDirection: 'column',
              gap: 26,
            }}
          >
            {column.map((cluster) => (
              <ClusterSection key={cluster.name} cluster={cluster} />
            ))}
          </div>
        ))}
      </div>

      <div
        style={{
          fontFamily: 'var(--font-body)',
          fontSize: 13,
          color: 'var(--color-text-dim)',
          textAlign: 'center',
          letterSpacing: '0.04em',
        }}
      >
        25 files · each with a single responsibility · average ~90 LOC each.
      </div>
    </SlideFrame>
  );
}

function ClusterSection({ cluster }: { cluster: ModuleCluster }) {
  return (
    <div style={{ display: 'flex', flexDirection: 'column', gap: 10 }}>
      <div
        style={{
          display: 'flex',
          alignItems: 'center',
          gap: 10,
          paddingBottom: 6,
          borderBottom: `1px solid ${cluster.color}44`,
        }}
      >
        <span
          aria-hidden
          style={{
            width: 8,
            height: 8,
            borderRadius: 999,
            background: cluster.color,
            boxShadow: `0 0 12px ${cluster.color}99`,
          }}
        />
        <div
          style={{
            fontFamily: 'var(--font-display)',
            fontWeight: 700,
            fontSize: 14,
            textTransform: 'uppercase',
            letterSpacing: '0.18em',
            color: cluster.color,
          }}
        >
          {cluster.name}
        </div>
      </div>
      <div style={{ display: 'flex', flexDirection: 'column', gap: 8 }}>
        {cluster.modules.map((module) => (
          <div
            key={module.name}
            style={{
              display: 'grid',
              gridTemplateColumns: '150px 1fr',
              alignItems: 'baseline',
              columnGap: 14,
            }}
          >
            <div
              style={{
                fontFamily: 'var(--font-mono)',
                fontSize: 13,
                color: 'var(--color-text-primary)',
                fontWeight: 500,
                letterSpacing: '-0.01em',
                whiteSpace: 'nowrap',
                overflow: 'hidden',
                textOverflow: 'ellipsis',
              }}
            >
              {module.name}
            </div>
            <div
              style={{
                fontFamily: 'var(--font-body)',
                fontSize: 12.5,
                color: 'var(--color-text-muted)',
                lineHeight: 1.45,
              }}
            >
              {module.description}
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}

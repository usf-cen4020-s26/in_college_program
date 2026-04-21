import { Player } from '@remotion/player';
import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { ArchGraph } from '../remotion/compositions';
import type { ArchGraphCluster } from '../remotion/compositions';
import { MODULE_CLUSTERS } from '../data/modules';

const FPS = 30;
const DURATION_IN_FRAMES = 300;

// main.cob anchors the center, so filter it out of any cluster's node list
// to avoid drawing the same label twice.
const CLUSTERS: ArchGraphCluster[] = MODULE_CLUSTERS.map((cluster) => ({
  label: cluster.name,
  color: cluster.color,
  nodes: cluster.modules
    .map((module) => module.name)
    .filter((name) => name !== 'main.cob'),
})).filter((cluster) => cluster.nodes.length > 0);

/**
 * Slide 13 — Trevor — "25 modules. One COBOL program".
 *
 * The whole slide is the ArchGraph constellation: main.cob at the
 * center, 24 copybooks orbiting in brand-colored clusters. No side
 * panels — the graph is the message.
 */
export function Slide13Architecture(_props: SlideProps) {
  return (
    <SlideFrame act="ACT III · HOW WE BUILT IT" kicker="ARCHITECTURE">
      <div style={{ display: 'flex', flexDirection: 'column', gap: 12 }}>
        <h1
          style={{
            margin: 0,
            fontFamily: 'var(--font-display)',
            fontSize: 82,
            fontWeight: 700,
            letterSpacing: '-0.02em',
            color: 'var(--color-text-primary)',
            lineHeight: 1.02,
          }}
        >
          25 modules. <span className="text-gradient">One COBOL program.</span>
        </h1>
        <p
          style={{
            margin: 0,
            fontFamily: 'var(--font-body)',
            fontSize: 22,
            color: 'var(--color-text-muted)',
            lineHeight: 1.4,
          }}
        >
          <span
            style={{
              fontFamily: 'var(--font-mono)',
              color: 'var(--color-brand-accent)',
            }}
          >
            main.cob
          </span>{' '}
          at the center. 24 copybooks clustered by concern.
        </p>
      </div>

      <div
        style={{
          flex: 1,
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          minHeight: 0,
        }}
      >
        <div
          style={{
            width: '82%',
            height: '100%',
            maxHeight: 760,
          }}
        >
          <Player
            component={ArchGraph}
            inputProps={{
              center: 'main.cob',
              clusters: CLUSTERS,
            }}
            durationInFrames={DURATION_IN_FRAMES}
            fps={FPS}
            compositionWidth={1920}
            compositionHeight={1080}
            autoPlay
            loop
            controls={false}
            clickToPlay={false}
            style={{ width: '100%', height: '100%' }}
          />
        </div>
      </div>
    </SlideFrame>
  );
}

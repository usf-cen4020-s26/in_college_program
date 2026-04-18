import { Player } from '@remotion/player';
import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { ArchGraph } from '../remotion/compositions';
import type { ArchGraphCluster } from '../remotion/compositions';
import { MODULE_CLUSTERS } from '../data/modules';

const FPS = 30;
const DURATION_IN_FRAMES = 300;

const CLUSTERS: ArchGraphCluster[] = MODULE_CLUSTERS.map((cluster) => ({
  label: cluster.name,
  color: cluster.color,
  nodes: cluster.modules
    .map((module) => module.name)
    .filter((name) => name !== 'main.cob'),
})).filter((cluster) => cluster.nodes.length > 0);

/**
 * Slide 43 — The Copybook Map: 24 copybooks, one orchestrator.
 */
export function Slide43CopybookMap(_props: SlideProps) {
  return (
    <SlideFrame act="INTERLUDE" kicker="MODULARIZATION · ARCHITECTURE">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(36px, 4em, 68px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
          lineHeight: 1.05,
        }}
      >
        24 Copybooks, <span className="text-gradient">One Orchestrator</span>
      </h1>

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
          data-record-dwell-ms={Math.ceil((DURATION_IN_FRAMES / FPS) * 1000)}
          style={{ width: '100%', height: '100%', maxHeight: 760 }}
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

import { SlideFrame } from '../components/SlideFrame';
import { CodePanel } from '../components/CodePanel';
import { SNIPPETS } from '../data/snippets';
import type { SlideProps } from './types';

const PSEUDOCODE = `valid = (A == me AND B == them)
     OR (B == me AND A == them)`;

export function Slide17CodeGemBidirectional(_props: SlideProps) {
  return (
    <SlideFrame act="ACT III · HOW WE BUILT IT" kicker="CODE GEM · GRAPH">
      <div>
        <h1
          style={{
            margin: 0,
            fontFamily: 'var(--font-display)',
            fontWeight: 700,
            fontSize: 68,
            lineHeight: 1.05,
            letterSpacing: '-0.02em',
            color: 'var(--color-text-primary)',
          }}
        >
          Connections are undirected — the code knows it.
        </h1>
      </div>

      <div
        style={{
          display: 'grid',
          gridTemplateColumns: '3fr 2fr',
          gap: 48,
          alignItems: 'start',
          flex: 1,
          minHeight: 0,
        }}
      >
        <div style={{ minWidth: 0 }}>
          <CodePanel
            filePath="src/SENDMESSAGE.cpy:198-225"
            code={SNIPPETS.bidirectional}
            maxHeight={640}
          />
        </div>

        <div style={{ display: 'flex', flexDirection: 'column', gap: 24 }}>
          <BidirectionalGraph />

          <div
            style={{
              fontFamily: 'var(--font-body)',
              fontSize: 15,
              lineHeight: 1.55,
              color: 'var(--color-text-muted)',
              padding: '18px 22px',
              borderRadius: 14,
              background: 'var(--color-bg-panel)',
              border: '1px solid rgba(112,181,249,0.14)',
            }}
          >
            Connection pairs are stored unordered, so{' '}
            <code
              style={{
                fontFamily: 'var(--font-mono)',
                fontSize: 13,
                color: 'var(--color-brand-accent)',
                background: 'rgba(10,102,194,0.18)',
                padding: '1px 6px',
                borderRadius: 4,
              }}
            >
              7820-VALIDATE-RECIPIENT
            </code>{' '}
            walks the table checking{' '}
            <strong style={{ color: 'var(--color-text-primary)' }}>
              both directions
            </strong>{' '}
            — (A→B) and (B→A). No false negatives from insertion order.
          </div>

          <div
            style={{
              padding: '20px 24px',
              borderRadius: 14,
              background: 'var(--color-bg-code)',
              border: '1px solid var(--color-bg-code-border)',
              fontFamily: 'var(--font-mono)',
              fontSize: 16,
              lineHeight: 1.6,
              color: 'var(--color-brand-accent)',
              whiteSpace: 'pre',
              boxShadow:
                '0 20px 40px -20px rgba(0,0,0,0.7), inset 0 1px 0 rgba(255,255,255,0.03)',
            }}
          >
            {PSEUDOCODE}
          </div>
        </div>
      </div>

      <div
        style={{
          fontFamily: 'var(--font-body)',
          fontSize: 14,
          color: 'var(--color-text-dim)',
          letterSpacing: '0.02em',
        }}
      >
        The same pattern is used by the messaging send flow, the network
        display, and the connection-acceptance handler.
      </div>
    </SlideFrame>
  );
}

function BidirectionalGraph() {
  return (
    <div
      style={{
        padding: '24px 20px',
        borderRadius: 18,
        background: 'var(--color-bg-panel)',
        border: '1px solid rgba(112,181,249,0.14)',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
      }}
    >
      <svg width="100%" viewBox="0 0 560 180" style={{ maxWidth: 560 }}>
        <defs>
          <marker
            id="bi-arrow-right"
            viewBox="0 0 10 10"
            refX="9"
            refY="5"
            markerWidth="6"
            markerHeight="6"
            orient="auto"
          >
            <path d="M 0 0 L 10 5 L 0 10 z" fill="#70B5F9" />
          </marker>
          <marker
            id="bi-arrow-left"
            viewBox="0 0 10 10"
            refX="1"
            refY="5"
            markerWidth="6"
            markerHeight="6"
            orient="auto"
          >
            <path d="M 10 0 L 0 5 L 10 10 z" fill="#70B5F9" />
          </marker>
        </defs>

        {/* Top arrow A -> B */}
        <line
          x1={140}
          y1={78}
          x2={420}
          y2={78}
          stroke="#70B5F9"
          strokeWidth={3}
          markerEnd="url(#bi-arrow-right)"
          style={{
            strokeDasharray: 600,
            strokeDashoffset: 600,
            animation: 'bi-draw 900ms 200ms forwards cubic-bezier(0.16,1,0.3,1)',
          }}
        />
        {/* Bottom arrow B -> A */}
        <line
          x1={420}
          y1={108}
          x2={140}
          y2={108}
          stroke="#70B5F9"
          strokeWidth={3}
          markerEnd="url(#bi-arrow-left)"
          style={{
            strokeDasharray: 600,
            strokeDashoffset: 600,
            animation: 'bi-draw 900ms 600ms forwards cubic-bezier(0.16,1,0.3,1)',
          }}
        />

        {/* Node A */}
        <circle
          cx={90}
          cy={90}
          r={50}
          fill="rgba(10,102,194,0.22)"
          stroke="#70B5F9"
          strokeWidth={2.5}
        />
        <text
          x={90}
          y={97}
          textAnchor="middle"
          fill="#F5F7FA"
          fontFamily='"IBM Plex Sans", sans-serif'
          fontSize={40}
          fontWeight={700}
        >
          A
        </text>

        {/* Node B */}
        <circle
          cx={470}
          cy={90}
          r={50}
          fill="rgba(10,102,194,0.22)"
          stroke="#70B5F9"
          strokeWidth={2.5}
        />
        <text
          x={470}
          y={97}
          textAnchor="middle"
          fill="#F5F7FA"
          fontFamily='"IBM Plex Sans", sans-serif'
          fontSize={40}
          fontWeight={700}
        >
          B
        </text>

        <style>{`
          @keyframes bi-draw {
            to { stroke-dashoffset: 0; }
          }
        `}</style>
      </svg>
    </div>
  );
}

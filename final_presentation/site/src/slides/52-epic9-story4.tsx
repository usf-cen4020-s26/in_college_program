import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';

const TERMINAL_BOX: React.CSSProperties = {
  flex: 1,
  borderRadius: 16,
  background: 'var(--color-bg-panel)',
  border: '1px solid rgba(112,181,249,0.14)',
  boxShadow: '0 24px 48px -28px rgba(0,0,0,0.75)',
  padding: '24px 28px',
  display: 'flex',
  flexDirection: 'column' as const,
  gap: 14,
};

const HEADER: React.CSSProperties = {
  fontFamily: 'var(--font-display)',
  fontWeight: 700,
  fontSize: 'clamp(18px, 1.5em, 26px)',
  color: 'var(--color-text-primary)',
  marginBottom: 8,
};

const LINE: React.CSSProperties = {
  fontFamily: 'var(--font-mono)',
  fontSize: 'clamp(13px, 0.95em, 16px)',
  color: 'var(--color-text-muted)',
  lineHeight: 1.6,
};

/**
 * Slide 52 — Story 4: Persistence across sessions.
 */
export function Slide52Story4(_props: SlideProps) {
  return (
    <SlideFrame act="PART C · EPIC 9 DEEP DIVE" kicker="STORY 4 · PERSISTENCE">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(48px, 5.5em, 88px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          lineHeight: 1.05,
        }}
      >
        <span className="text-gradient">Across Sessions</span>
      </h1>

      <div
        style={{
          flex: 1,
          display: 'flex',
          alignItems: 'center',
          gap: 32,
        }}
      >
        {/* Session 1 */}
        <div style={TERMINAL_BOX}>
          <div style={HEADER}>Session 1</div>
          <div style={LINE}>alice logs in</div>
          <div style={LINE}>Sends message to bob</div>
          <div style={{ ...LINE, color: 'var(--color-brand-accent)' }}>
            &#8594; Written to MESSAGES.DAT
          </div>
          <div style={LINE}>alice logs out</div>
        </div>

        {/* Arrow */}
        <div
          style={{
            display: 'flex',
            flexDirection: 'column',
            alignItems: 'center',
            gap: 12,
            flexShrink: 0,
          }}
        >
          <div
            style={{
              width: 80,
              height: 4,
              borderRadius: 2,
              background: 'linear-gradient(90deg, #0A66C2, #38BDF8)',
            }}
          />
          <span
            style={{
              fontFamily: 'var(--font-mono)',
              fontSize: 'clamp(11px, 0.85em, 14px)',
              color: 'var(--color-brand-accent)',
              fontWeight: 600,
              letterSpacing: '0.04em',
              textAlign: 'center',
              whiteSpace: 'nowrap',
            }}
          >
            MESSAGES.DAT
            <br />
            persists
          </span>
          <div
            style={{
              width: 80,
              height: 4,
              borderRadius: 2,
              background: 'linear-gradient(90deg, #0A66C2, #38BDF8)',
            }}
          />
        </div>

        {/* Session 2 */}
        <div style={TERMINAL_BOX}>
          <div style={HEADER}>Session 2</div>
          <div style={LINE}>bob logs in</div>
          <div style={LINE}>Opens View Messages</div>
          <div style={{ ...LINE, color: '#38BDF8' }}>
            &#8594; Reads alice's message
          </div>
          <div style={LINE}>Message persisted across restart</div>
        </div>
      </div>
    </SlideFrame>
  );
}

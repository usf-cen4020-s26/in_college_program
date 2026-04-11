import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { CodePanel } from '../components/CodePanel';

const COBOL_CODE = `IF MSG-RECIPIENT = WS-CURRENT-USER
    PERFORM 8000-WRITE-OUTPUT
END-IF`;

const USERS = [
  { name: 'alice', color: 'rgba(112,181,249,0.25)', borderColor: 'rgba(112,181,249,0.5)' },
  { name: 'bob', color: 'rgba(56,189,248,0.35)', borderColor: '#38BDF8' },
  { name: 'alice', color: 'rgba(112,181,249,0.25)', borderColor: 'rgba(112,181,249,0.5)' },
  { name: 'bob', color: 'rgba(56,189,248,0.35)', borderColor: '#38BDF8' },
  { name: 'charlie', color: 'rgba(168,85,247,0.25)', borderColor: 'rgba(168,85,247,0.5)' },
  { name: 'bob', color: 'rgba(56,189,248,0.35)', borderColor: '#38BDF8' },
];

/**
 * Slide 51 — Story 3: Recipient Isolation.
 */
export function Slide51Story3(_props: SlideProps) {
  return (
    <SlideFrame act="PART C · EPIC 9 DEEP DIVE" kicker="STORY 3 · ISOLATION">
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
        <span className="text-gradient">Your Messages Only</span>
      </h1>

      <div
        style={{
          flex: 1,
          display: 'grid',
          gridTemplateColumns: '1fr 1fr',
          gap: 48,
          alignItems: 'center',
        }}
      >
        {/* Diagram: MESSAGES.DAT with filter */}
        <div
          style={{
            display: 'flex',
            flexDirection: 'column',
            gap: 16,
          }}
        >
          <div
            style={{
              fontFamily: 'var(--font-mono)',
              fontWeight: 700,
              fontSize: 'clamp(16px, 1.3em, 22px)',
              color: 'var(--color-text-primary)',
              marginBottom: 8,
            }}
          >
            MESSAGES.DAT
          </div>

          {USERS.map((user, i) => (
            <div
              key={`${user.name}-${i}`}
              style={{
                padding: '10px 18px',
                borderRadius: 10,
                background: user.color,
                border: `2px solid ${user.borderColor}`,
                fontFamily: 'var(--font-mono)',
                fontSize: 'clamp(13px, 1em, 16px)',
                color: 'var(--color-text-primary)',
                opacity: user.name === 'bob' ? 1 : 0.35,
                transition: 'opacity 0.3s',
                display: 'flex',
                alignItems: 'center',
                gap: 12,
              }}
            >
              <span style={{ fontWeight: 700 }}>To: {user.name}</span>
              <span style={{ color: 'var(--color-text-muted)', fontSize: '0.85em' }}>
                "Message #{i + 1}..."
              </span>
              {user.name === 'bob' && (
                <span
                  style={{
                    marginLeft: 'auto',
                    fontFamily: 'var(--font-body)',
                    fontSize: 11,
                    fontWeight: 700,
                    letterSpacing: '0.15em',
                    color: '#38BDF8',
                    textTransform: 'uppercase',
                  }}
                >
                  MATCH
                </span>
              )}
            </div>
          ))}
        </div>

        {/* Code snippet */}
        <div>
          <CodePanel
            filePath="src/MSG.cpy"
            code={COBOL_CODE}
            lang="cobol"
          />
        </div>
      </div>
    </SlideFrame>
  );
}

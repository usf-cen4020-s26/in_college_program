import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';

const FIELDS = [
  { name: 'ID', bytes: 5, color: '#0A66C2' },
  { name: 'Sender', bytes: 20, color: '#38BDF8' },
  { name: 'Recipient', bytes: 20, color: '#6366F1' },
  { name: 'Content', bytes: 200, color: '#22D3EE' },
  { name: 'Timestamp', bytes: 20, color: '#A78BFA' },
];

const TOTAL = FIELDS.reduce((sum, f) => sum + f.bytes, 0);

/**
 * Slide 47 — MESSAGES.DAT record layout.
 * Horizontal colored bar showing field sizes.
 */
export function Slide47M8Data(_props: SlideProps) {
  return (
    <SlideFrame act="PART B · BUILDING INCOLLEGE" kicker="EPIC #8 · DATA">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(36px, 4em, 64px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          lineHeight: 1.05,
        }}
      >
        <span className="text-gradient">MESSAGES.DAT</span>
      </h1>

      <div
        style={{
          flex: 1,
          display: 'flex',
          flexDirection: 'column',
          justifyContent: 'center',
          gap: 48,
        }}
      >
        {/* Horizontal record bar */}
        <div
          style={{
            display: 'flex',
            borderRadius: 16,
            overflow: 'hidden',
            height: 80,
            boxShadow: '0 20px 60px -20px rgba(0,0,0,0.7)',
          }}
        >
          {FIELDS.map((field) => (
            <div
              key={field.name}
              style={{
                flex: field.bytes,
                background: field.color,
                display: 'flex',
                flexDirection: 'column',
                alignItems: 'center',
                justifyContent: 'center',
                gap: 4,
                borderRight: '2px solid rgba(0,0,0,0.3)',
                padding: '0 4px',
              }}
            >
              <span
                style={{
                  fontFamily: 'var(--font-display)',
                  fontWeight: 700,
                  fontSize: 'clamp(11px, 1em, 16px)',
                  color: '#fff',
                  textShadow: '0 1px 3px rgba(0,0,0,0.5)',
                }}
              >
                {field.name}
              </span>
              <span
                style={{
                  fontFamily: 'var(--font-mono)',
                  fontSize: 'clamp(10px, 0.8em, 13px)',
                  color: 'rgba(255,255,255,0.85)',
                }}
              >
                {field.bytes}B
              </span>
            </div>
          ))}
        </div>

        {/* Field legend */}
        <div
          style={{
            display: 'flex',
            gap: 32,
            justifyContent: 'center',
            flexWrap: 'wrap',
          }}
        >
          {FIELDS.map((field) => (
            <div
              key={field.name}
              style={{
                display: 'flex',
                alignItems: 'center',
                gap: 10,
              }}
            >
              <div
                style={{
                  width: 16,
                  height: 16,
                  borderRadius: 4,
                  background: field.color,
                }}
              />
              <span
                style={{
                  fontFamily: 'var(--font-mono)',
                  fontSize: 'clamp(13px, 1em, 16px)',
                  color: 'var(--color-text-muted)',
                }}
              >
                {field.name} ({field.bytes} bytes)
              </span>
            </div>
          ))}
        </div>

        {/* Total */}
        <div
          style={{
            textAlign: 'center',
            fontFamily: 'var(--font-display)',
            fontSize: 'clamp(24px, 2.5em, 42px)',
            fontWeight: 700,
            color: 'var(--color-text-primary)',
          }}
        >
          <span className="text-gradient">{TOTAL} bytes</span>
          <span
            style={{
              fontWeight: 400,
              fontSize: '0.6em',
              color: 'var(--color-text-muted)',
              marginLeft: 16,
            }}
          >
            per record
          </span>
        </div>
      </div>
    </SlideFrame>
  );
}

import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';

const BOXES = [
  {
    name: 'main.cob',
    detail: '520 lines — monolithic COBOL source with all logic',
  },
  {
    name: 'ACCOUNTS.DAT',
    detail: 'Flat file storing usernames and hashed passwords',
  },
  {
    name: 'test_runner.py',
    detail: 'Python script discovering and running .in/.out fixtures',
  },
];

/**
 * Slide 12 — Architecture: Week 1.
 * Three simple boxes showing the entire codebase.
 */
export function Slide12M1Architecture(_props: SlideProps) {
  return (
    <SlideFrame act="PART B · BUILDING INCOLLEGE" kicker="WEEK 1 · ARCHITECTURE">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(48px, 5.5em, 88px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
          lineHeight: 1.05,
        }}
      >
        Starting Small
      </h1>

      <div
        style={{
          display: 'grid',
          gridTemplateColumns: 'repeat(3, 1fr)',
          gap: 32,
          flex: 1,
          alignItems: 'center',
        }}
      >
        {BOXES.map((box) => (
          <div
            key={box.name}
            style={{
              padding: '40px 36px',
              borderRadius: 20,
              background: 'var(--color-bg-card)',
              border: '1px solid rgba(112,181,249,0.12)',
              boxShadow:
                '0 30px 60px -32px rgba(0,0,0,0.8), inset 0 1px 0 rgba(255,255,255,0.04)',
              display: 'flex',
              flexDirection: 'column',
              gap: 16,
              textAlign: 'center',
            }}
          >
            <div
              style={{
                fontFamily: 'var(--font-mono)',
                fontSize: 'clamp(18px, 1.6em, 28px)',
                fontWeight: 700,
                color: 'var(--color-brand-accent)',
                letterSpacing: '-0.01em',
              }}
            >
              {box.name}
            </div>
            <p
              style={{
                margin: 0,
                fontFamily: 'var(--font-body)',
                fontSize: 'clamp(13px, 1em, 16px)',
                lineHeight: 1.5,
                color: 'var(--color-text-muted)',
              }}
            >
              {box.detail}
            </p>
          </div>
        ))}
      </div>
    </SlideFrame>
  );
}

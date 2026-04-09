import type { SlideProps } from './types';

export function Slide16CodeGemViewLoop(_props: SlideProps) {
  return (
    <div className="deck-slide" style={{ background: 'var(--color-bg-base)' }}>
      <div
        style={{
          position: 'absolute',
          inset: 0,
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'center',
          justifyContent: 'center',
          padding: 96,
          gap: 16,
        }}
      >
        <span style={{ fontSize: 14, letterSpacing: '0.3em', color: '#5F6B82', textTransform: 'uppercase' }}>
          ACT III · HOW WE BUILT IT
        </span>
        <h1 style={{ margin: 0, fontFamily: 'var(--font-display)', fontSize: 96, fontWeight: 700, color: '#F5F7FA' }}>
          Recursive Message Read
        </h1>
        <p style={{ margin: 0, fontSize: 18, color: '#5F6B82' }}>slide 16 stub</p>
      </div>
    </div>
  );
}

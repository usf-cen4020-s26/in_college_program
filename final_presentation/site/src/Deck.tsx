import { useCallback, useEffect, useMemo, useState } from 'react';
import { SLIDES } from './slides';
import { deck } from './theme';

/**
 * Deck shell.
 *
 * Owns:
 *   - current slide index (0-based)
 *   - keyboard navigation (← → Home End 1-9 F)
 *   - URL sync via ?slide=N (1-based in the URL)
 *   - fullscreen toggle via browser API
 *   - the 16:9 stage container
 *
 * Slides are kept in src/slides/index.ts so teammates can edit one
 * slide without touching this shell.
 */
export function Deck() {
  const total = SLIDES.length;
  const [index, setIndex] = useState<number>(() => readIndexFromUrl(total));
  const [uiHidden, setUiHidden] = useState(false);

  const clamp = useCallback(
    (next: number) => Math.max(0, Math.min(total - 1, next)),
    [total],
  );

  const goto = useCallback(
    (next: number) => {
      setIndex((current) => {
        const clamped = clamp(next);
        if (clamped === current) return current;
        writeIndexToUrl(clamped);
        return clamped;
      });
    },
    [clamp],
  );

  const prev = useCallback(() => goto(index - 1), [goto, index]);
  const next = useCallback(() => goto(index + 1), [goto, index]);

  // Keyboard navigation
  useEffect(() => {
    const onKey = (e: KeyboardEvent) => {
      // Ignore modifier combinations so the browser shortcuts still work
      if (e.metaKey || e.ctrlKey || e.altKey) return;

      switch (e.key) {
        case 'ArrowRight':
        case 'PageDown':
        case ' ':
        case 'n':
          e.preventDefault();
          next();
          break;
        case 'ArrowLeft':
        case 'PageUp':
        case 'p':
          e.preventDefault();
          prev();
          break;
        case 'Home':
          e.preventDefault();
          goto(0);
          break;
        case 'End':
          e.preventDefault();
          goto(total - 1);
          break;
        case 'f':
        case 'F':
          e.preventDefault();
          toggleFullscreen();
          break;
        case 'h':
        case 'H':
          e.preventDefault();
          setUiHidden((v) => !v);
          break;
        default:
          // number shortcuts jump to slide (1 = first)
          if (/^[1-9]$/.test(e.key)) {
            e.preventDefault();
            const target = Number(e.key) - 1;
            if (target < total) goto(target);
          }
      }
    };
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  }, [goto, next, prev, total]);

  // React to back/forward buttons
  useEffect(() => {
    const onPopState = () => setIndex(readIndexFromUrl(total));
    window.addEventListener('popstate', onPopState);
    return () => window.removeEventListener('popstate', onPopState);
  }, [total]);

  const slide = SLIDES[index];
  const Component = slide.component;

  const bg = useMemo(
    () => ({
      ['--deck-index' as string]: String(index + 1),
      ['--deck-total' as string]: String(total),
    }),
    [index, total],
  );

  return (
    <div className="deck-root" style={bg}>
      <div
        className="deck-stage"
        style={{ fontSize: `calc(min(100vw, 100vh * ${deck.width} / ${deck.height}) / ${deck.width} * 16)` }}
      >
        <Component slideIndex={index + 1} totalSlides={total} />
        {!uiHidden && (
          <div
            className="pointer-events-none absolute inset-x-0 bottom-0 flex items-end justify-between px-6 pb-4 text-[0.72rem] font-medium tracking-[0.2em] uppercase text-text-dim"
            style={{
              color: 'var(--color-text-dim)',
              fontSize: '12px',
              textShadow: '0 1px 0 rgba(0,0,0,0.6)',
            }}
          >
            <span>{slide.act}</span>
            <span>
              {String(index + 1).padStart(2, '0')} / {String(total).padStart(2, '0')}
            </span>
          </div>
        )}
      </div>
      {!uiHidden && (
        <DeckControls
          index={index}
          total={total}
          onPrev={prev}
          onNext={next}
          onToggleUi={() => setUiHidden((v) => !v)}
          speaker={slide.speaker}
        />
      )}
    </div>
  );
}

function DeckControls(props: {
  index: number;
  total: number;
  onPrev: () => void;
  onNext: () => void;
  onToggleUi: () => void;
  speaker: string;
}) {
  return (
    <div
      style={{
        position: 'fixed',
        left: '50%',
        bottom: 16,
        transform: 'translateX(-50%)',
        display: 'flex',
        alignItems: 'center',
        gap: 12,
        padding: '8px 14px',
        background: 'rgba(8, 14, 28, 0.72)',
        backdropFilter: 'blur(12px)',
        WebkitBackdropFilter: 'blur(12px)',
        border: '1px solid rgba(112,181,249,0.18)',
        borderRadius: 999,
        color: '#C9D4E6',
        fontSize: 12,
        fontFamily: 'var(--font-body)',
        letterSpacing: '0.02em',
        opacity: 0.7,
        transition: 'opacity 160ms ease-out',
      }}
      onMouseEnter={(e) => (e.currentTarget.style.opacity = '1')}
      onMouseLeave={(e) => (e.currentTarget.style.opacity = '0.7')}
    >
      <button
        onClick={props.onPrev}
        disabled={props.index === 0}
        style={{
          padding: '4px 10px',
          borderRadius: 999,
          color: props.index === 0 ? '#4A5468' : '#E3ECF7',
        }}
        aria-label="Previous slide"
      >
        ←
      </button>
      <span style={{ fontVariantNumeric: 'tabular-nums' }}>
        {String(props.index + 1).padStart(2, '0')} / {String(props.total).padStart(2, '0')}
      </span>
      <span style={{ color: '#5F6B82' }}>·</span>
      <span>{props.speaker}</span>
      <button
        onClick={props.onNext}
        disabled={props.index === props.total - 1}
        style={{
          padding: '4px 10px',
          borderRadius: 999,
          color: props.index === props.total - 1 ? '#4A5468' : '#E3ECF7',
        }}
        aria-label="Next slide"
      >
        →
      </button>
      <span style={{ color: '#5F6B82' }}>·</span>
      <button
        onClick={toggleFullscreen}
        style={{ padding: '4px 10px', borderRadius: 999, color: '#E3ECF7' }}
        aria-label="Toggle fullscreen"
        title="Fullscreen (F)"
      >
        ⛶
      </button>
      <button
        onClick={props.onToggleUi}
        style={{ padding: '4px 10px', borderRadius: 999, color: '#E3ECF7' }}
        aria-label="Hide chrome"
        title="Hide chrome (H)"
      >
        ◐
      </button>
    </div>
  );
}

function readIndexFromUrl(total: number): number {
  if (typeof window === 'undefined') return 0;
  const raw = new URLSearchParams(window.location.search).get('slide');
  if (!raw) return 0;
  const n = Number.parseInt(raw, 10);
  if (!Number.isFinite(n)) return 0;
  return Math.max(0, Math.min(total - 1, n - 1));
}

function writeIndexToUrl(zeroBased: number): void {
  if (typeof window === 'undefined') return;
  const params = new URLSearchParams(window.location.search);
  params.set('slide', String(zeroBased + 1));
  const url = `${window.location.pathname}?${params.toString()}`;
  window.history.pushState({ slide: zeroBased }, '', url);
}

function toggleFullscreen(): void {
  if (typeof document === 'undefined') return;
  if (document.fullscreenElement) {
    void document.exitFullscreen();
  } else {
    void document.documentElement.requestFullscreen();
  }
}

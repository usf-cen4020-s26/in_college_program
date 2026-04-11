import { useCallback, useEffect, useMemo, useState } from 'react';
import { SLIDES } from './slides';
import { deck } from './theme';

/**
 * Deck shell.
 *
 * Owns:
 *   - current slide index (0-based) + step index (0-based)
 *   - keyboard navigation (← → Home End digits F H)
 *   - URL sync via ?slide=N&step=M (1-based in URL)
 *   - fullscreen toggle
 *   - breakout mode (some slides fill the viewport)
 *   - the responsive 16:9 stage container
 */
export function Deck() {
  const total = SLIDES.length;
  const [index, setIndex] = useState<number>(() => readFromUrl(total).slide);
  const [stepIdx, setStepIdx] = useState<number>(() => readFromUrl(total).step);
  const [uiHidden, setUiHidden] = useState(false);

  const slide = SLIDES[index];
  const maxSteps = slide.steps ?? 1;
  const isBreakout = slide.breakout ?? false;

  const clampSlide = useCallback(
    (n: number) => Math.max(0, Math.min(total - 1, n)),
    [total],
  );

  const gotoSlide = useCallback(
    (nextSlide: number, nextStep = 0) => {
      const clamped = clampSlide(nextSlide);
      setIndex(clamped);
      const clampedStep = Math.max(
        0,
        Math.min((SLIDES[clamped].steps ?? 1) - 1, nextStep),
      );
      setStepIdx(clampedStep);
      writeToUrl(clamped, clampedStep);
    },
    [clampSlide],
  );

  const advance = useCallback(() => {
    if (stepIdx < maxSteps - 1) {
      const next = stepIdx + 1;
      setStepIdx(next);
      writeToUrl(index, next);
    } else if (index < total - 1) {
      gotoSlide(index + 1, 0);
    }
  }, [stepIdx, maxSteps, index, total, gotoSlide]);

  const retreat = useCallback(() => {
    if (stepIdx > 0) {
      const prev = stepIdx - 1;
      setStepIdx(prev);
      writeToUrl(index, prev);
    } else if (index > 0) {
      const prevSlide = index - 1;
      const prevMaxSteps = (SLIDES[prevSlide].steps ?? 1) - 1;
      gotoSlide(prevSlide, prevMaxSteps);
    }
  }, [stepIdx, index, gotoSlide]);

  // Keyboard navigation
  useEffect(() => {
    const onKey = (e: KeyboardEvent) => {
      if (e.metaKey || e.ctrlKey || e.altKey) return;

      switch (e.key) {
        case 'ArrowRight':
        case 'PageDown':
        case ' ':
        case 'n':
          e.preventDefault();
          advance();
          break;
        case 'ArrowLeft':
        case 'PageUp':
        case 'p':
          e.preventDefault();
          retreat();
          break;
        case 'Home':
          e.preventDefault();
          gotoSlide(0, 0);
          break;
        case 'End':
          e.preventDefault();
          gotoSlide(total - 1, 0);
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
          if (/^[1-9]$/.test(e.key)) {
            e.preventDefault();
            const target = Number(e.key) - 1;
            if (target < total) gotoSlide(target, 0);
          }
      }
    };
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  }, [advance, retreat, gotoSlide, total]);

  // Popstate (back/forward buttons)
  useEffect(() => {
    const onPop = () => {
      const { slide: s, step: st } = readFromUrl(total);
      setIndex(s);
      setStepIdx(st);
    };
    window.addEventListener('popstate', onPop);
    return () => window.removeEventListener('popstate', onPop);
  }, [total]);

  const Component = slide.component;

  const cssVars = useMemo(
    () => ({
      ['--deck-index' as string]: String(index + 1),
      ['--deck-total' as string]: String(total),
    }),
    [index, total],
  );

  return (
    <div className="deck-root" style={cssVars}>
      <div
        className={`deck-stage${isBreakout ? ' breakout' : ''}`}
        style={{
          fontSize: `clamp(10px, calc(min(100vw, 100vh * ${deck.width} / ${deck.height}) / ${deck.width} * 16), 16px)`,
        }}
      >
        <Component slideIndex={index + 1} totalSlides={total} step={stepIdx} />
        {!uiHidden && (
          <div
            className="pointer-events-none absolute inset-x-0 bottom-0 flex items-end justify-between px-6 pb-4"
            style={{
              color: 'var(--color-text-dim)',
              fontSize: '12px',
              fontWeight: 500,
              letterSpacing: '0.2em',
              textTransform: 'uppercase',
              textShadow: '0 1px 0 rgba(0,0,0,0.6)',
            }}
          >
            <span>{slide.act}</span>
            <span>
              {String(index + 1).padStart(2, '0')} / {String(total).padStart(2, '0')}
              {maxSteps > 1 && (
                <span style={{ opacity: 0.5 }}>
                  {' '}
                  · {stepIdx + 1}/{maxSteps}
                </span>
              )}
            </span>
          </div>
        )}
      </div>
      {!uiHidden && (
        <DeckControls
          index={index}
          total={total}
          stepIdx={stepIdx}
          maxSteps={maxSteps}
          onPrev={retreat}
          onNext={advance}
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
  stepIdx: number;
  maxSteps: number;
  onPrev: () => void;
  onNext: () => void;
  onToggleUi: () => void;
  speaker: string;
}) {
  const atStart = props.index === 0 && props.stepIdx === 0;
  const atEnd = props.index === props.total - 1 && props.stepIdx >= props.maxSteps - 1;

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
        border: '1px solid rgba(133,197,255,0.18)',
        borderRadius: 999,
        color: '#C9D4E6',
        fontSize: 12,
        fontFamily: 'var(--font-body)',
        letterSpacing: '0.02em',
        opacity: 0.7,
        transition: 'opacity 160ms ease-out',
        zIndex: 100,
      }}
      onMouseEnter={(e) => (e.currentTarget.style.opacity = '1')}
      onMouseLeave={(e) => (e.currentTarget.style.opacity = '0.7')}
    >
      <button
        onClick={props.onPrev}
        disabled={atStart}
        style={{
          padding: '4px 10px',
          borderRadius: 999,
          color: atStart ? '#4A5468' : '#E3ECF7',
        }}
        aria-label="Previous"
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
        disabled={atEnd}
        style={{
          padding: '4px 10px',
          borderRadius: 999,
          color: atEnd ? '#4A5468' : '#E3ECF7',
        }}
        aria-label="Next"
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

function readFromUrl(total: number): { slide: number; step: number } {
  if (typeof window === 'undefined') return { slide: 0, step: 0 };
  const params = new URLSearchParams(window.location.search);

  const rawSlide = params.get('slide');
  let slide = 0;
  if (rawSlide) {
    const n = Number.parseInt(rawSlide, 10);
    if (Number.isFinite(n)) slide = Math.max(0, Math.min(total - 1, n - 1));
  }

  const rawStep = params.get('step');
  let step = 0;
  if (rawStep) {
    const s = Number.parseInt(rawStep, 10);
    if (Number.isFinite(s)) {
      const maxSteps = (SLIDES[slide]?.steps ?? 1) - 1;
      step = Math.max(0, Math.min(maxSteps, s - 1));
    }
  }

  return { slide, step };
}

function writeToUrl(zeroBased: number, stepZeroBased: number): void {
  if (typeof window === 'undefined') return;
  const params = new URLSearchParams(window.location.search);
  params.set('slide', String(zeroBased + 1));
  if (stepZeroBased > 0) {
    params.set('step', String(stepZeroBased + 1));
  } else {
    params.delete('step');
  }
  const url = `${window.location.pathname}?${params.toString()}`;
  window.history.pushState({ slide: zeroBased, step: stepZeroBased }, '', url);
}

function toggleFullscreen(): void {
  if (typeof document === 'undefined') return;
  if (document.fullscreenElement) {
    void document.exitFullscreen();
  } else {
    void document.documentElement.requestFullscreen();
  }
}

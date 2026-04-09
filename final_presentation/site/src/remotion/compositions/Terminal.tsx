import { AbsoluteFill, useCurrentFrame, useVideoConfig } from 'remotion';
import { palette, typography } from '../../theme';

/**
 * A single step in a terminal playback.
 *
 * - `print` renders program output instantly (optionally waiting `delay` ms first).
 * - `type` simulates a user typing `text` char-by-char at `speed` ms/char (default 60).
 * - `pause` waits `ms` before the next event fires.
 * - `clear` wipes the visible scrollback.
 */
export type TerminalEvent =
  | { type: 'print'; text: string; delay?: number }
  | { type: 'type'; text: string; speed?: number }
  | { type: 'pause'; ms: number }
  | { type: 'clear' };

export type TerminalProps = {
  script: readonly TerminalEvent[];
  promptLabel?: string;
  title?: string;
  accent?: 'brand' | 'success' | 'warn';
};

type RenderedLine = {
  text: string;
  kind: 'output' | 'input';
};

const DEFAULT_TYPE_SPEED_MS = 60;
const VISIBLE_LINES = 28;
const LINE_HEIGHT_PX = 46;
const MONO_FONT_SIZE_PX = 30;
const CURSOR_ON_FRAMES = 6;
const CURSOR_OFF_FRAMES = 6;

const ACCENT_COLORS: Record<NonNullable<TerminalProps['accent']>, string> = {
  brand: palette.brand.accent,
  success: palette.state.success,
  warn: palette.state.warn,
};

/**
 * Deterministic terminal playback built from a script of events.
 *
 * Everything the user sees is a pure function of the current frame:
 * the elapsed time (in ms, derived from fps) is walked against the script
 * timeline to compute which events have fired and how much of the current
 * "type" event has been revealed.
 */
export function Terminal({
  script,
  promptLabel = '',
  title = 'incollege — session',
  accent = 'brand',
}: TerminalProps) {
  const frame = useCurrentFrame();
  const { fps } = useVideoConfig();
  const elapsedMs = (frame / fps) * 1000;
  const accentColor = ACCENT_COLORS[accent];

  const lines = renderScript(script, elapsedMs, promptLabel);

  // Cursor blink is anchored to frames so it matches the deck's fps exactly.
  const blinkPeriod = CURSOR_ON_FRAMES + CURSOR_OFF_FRAMES;
  const cursorVisible = frame % blinkPeriod < CURSOR_ON_FRAMES;

  // Auto-scroll: translate the content stack upward once it exceeds the
  // visible window so the newest line always sits at the bottom.
  const overflow = Math.max(0, lines.length - VISIBLE_LINES);
  const scrollOffsetPx = overflow * LINE_HEIGHT_PX;

  return (
    <AbsoluteFill
      style={{
        backgroundColor: palette.bg.base,
        padding: 64,
        fontFamily: typography.mono,
      }}
    >
      <div
        style={{
          position: 'relative',
          width: '100%',
          height: '100%',
          borderRadius: 18,
          overflow: 'hidden',
          backgroundColor: palette.bg.code,
          border: `1px solid ${palette.bg.codeBorder}`,
          boxShadow:
            '0 40px 120px rgba(0,0,0,0.6), inset 0 1px 0 rgba(255,255,255,0.04)',
          display: 'flex',
          flexDirection: 'column',
        }}
      >
        <TitleBar title={title} />
        <div
          style={{
            position: 'relative',
            flex: 1,
            overflow: 'hidden',
            padding: '32px 44px 44px 44px',
          }}
        >
          <div
            style={{
              transform: `translateY(${-scrollOffsetPx}px)`,
              transition: 'none',
            }}
          >
            {lines.map((line, idx) => {
              const isLast = idx === lines.length - 1;
              return (
                <LineRow
                  key={idx}
                  line={line}
                  promptLabel={promptLabel}
                  accentColor={accentColor}
                  showCursor={isLast && cursorVisible}
                />
              );
            })}
          </div>
        </div>
      </div>
    </AbsoluteFill>
  );
}

function TitleBar({ title }: { title: string }) {
  return (
    <div
      style={{
        display: 'flex',
        alignItems: 'center',
        gap: 10,
        height: 48,
        padding: '0 20px',
        backgroundColor: '#0A1020',
        borderBottom: `1px solid ${palette.bg.codeBorder}`,
      }}
    >
      <TrafficLight color="#FF5F57" />
      <TrafficLight color="#FEBC2E" />
      <TrafficLight color="#28C840" />
      <div
        style={{
          flex: 1,
          textAlign: 'center',
          color: palette.text.muted,
          fontFamily: typography.body,
          fontSize: 14,
          letterSpacing: '0.02em',
          transform: 'translateX(-30px)',
        }}
      >
        {title}
      </div>
    </div>
  );
}

function TrafficLight({ color }: { color: string }) {
  return (
    <div
      style={{
        width: 14,
        height: 14,
        borderRadius: '50%',
        backgroundColor: color,
        boxShadow: 'inset 0 0 0 0.5px rgba(0,0,0,0.3)',
      }}
    />
  );
}

function LineRow({
  line,
  promptLabel,
  accentColor,
  showCursor,
}: {
  line: RenderedLine;
  promptLabel: string;
  accentColor: string;
  showCursor: boolean;
}) {
  const isInput = line.kind === 'input';
  const showPrompt = isInput && promptLabel.length > 0;
  return (
    <div
      style={{
        display: 'flex',
        alignItems: 'baseline',
        minHeight: LINE_HEIGHT_PX,
        lineHeight: `${LINE_HEIGHT_PX}px`,
        fontSize: MONO_FONT_SIZE_PX,
        whiteSpace: 'pre',
        fontFamily: typography.mono,
      }}
    >
      {showPrompt && (
        <span
          style={{
            color: accentColor,
            marginRight: 4,
            textShadow: `0 0 12px ${accentColor}55`,
          }}
        >
          {promptLabel}
        </span>
      )}
      <span
        style={{
          color: isInput ? accentColor : '#E6EAF2',
        }}
      >
        {line.text}
      </span>
      {showCursor && (
        <span
          style={{
            display: 'inline-block',
            width: 14,
            height: MONO_FONT_SIZE_PX * 0.95,
            marginLeft: 4,
            backgroundColor: accentColor,
            boxShadow: `0 0 12px ${accentColor}88`,
            transform: 'translateY(4px)',
          }}
        />
      )}
    </div>
  );
}

/**
 * Walk the script against the elapsed ms and return the visible lines.
 *
 * The rules:
 *   - `pause` consumes time but never produces output.
 *   - `print` consumes its own `delay` (if any) then appears instantly.
 *     Each `\n` in its text starts a new line.
 *   - `type` consumes `speed` ms per character; characters are revealed
 *     one at a time. The current partial line lives on the prompt row.
 *   - `clear` resets the buffer instantly (no time cost).
 */
function renderScript(
  script: readonly TerminalEvent[],
  elapsedMs: number,
  _promptLabel: string,
): RenderedLine[] {
  let cursor = 0;
  let lines: RenderedLine[] = [];

  // Every "type" event opens a new input line; subsequent chars extend it.
  // We push a placeholder line at the start of each typing event so the
  // prompt prefix renders even before the first character appears.
  const pushInputLine = () => {
    lines.push({ text: '', kind: 'input' });
  };

  const appendToLastInput = (chunk: string) => {
    const parts = chunk.split('\n');
    const last = lines[lines.length - 1];
    if (!last || last.kind !== 'input') {
      pushInputLine();
    }
    // Append first segment to the current input line.
    lines[lines.length - 1] = {
      text: lines[lines.length - 1].text + parts[0],
      kind: 'input',
    };
    // Any additional segments start fresh lines (still tagged input so
    // continuation of a multi-line command stays visually coherent).
    for (let i = 1; i < parts.length; i++) {
      lines.push({ text: parts[i], kind: 'input' });
    }
  };

  const pushOutputText = (text: string) => {
    const parts = text.split('\n');
    for (const part of parts) {
      lines.push({ text: part, kind: 'output' });
    }
  };

  for (const event of script) {
    if (elapsedMs < cursor) break;

    switch (event.type) {
      case 'pause': {
        cursor += event.ms;
        break;
      }
      case 'print': {
        const delay = event.delay ?? 0;
        cursor += delay;
        if (elapsedMs < cursor) {
          // delay still running — nothing visible yet from this event
          return lines;
        }
        pushOutputText(event.text);
        break;
      }
      case 'clear': {
        lines = [];
        break;
      }
      case 'type': {
        const speed = event.speed ?? DEFAULT_TYPE_SPEED_MS;
        const chars = [...event.text];
        const totalDuration = chars.length * speed;

        // Start a new input line as soon as we enter the event.
        pushInputLine();

        const consumedSoFar = elapsedMs - cursor;
        if (consumedSoFar >= totalDuration) {
          // Fully typed — append the entire text.
          appendToLastInput(event.text);
          cursor += totalDuration;
        } else {
          // Partial: reveal only the characters whose time has elapsed.
          const revealed = Math.max(
            0,
            Math.floor(consumedSoFar / speed),
          );
          if (revealed > 0) {
            appendToLastInput(chars.slice(0, revealed).join(''));
          }
          // Event still in progress — stop walking further events.
          return lines;
        }
        break;
      }
    }
  }

  return lines;
}

import type { TerminalEvent } from './create-account';

/**
 * Mirrors the real `tests/live_cli.py` session: each user input line is
 * appended to `.live_session.input.txt`, the COBOL binary is re-run with
 * the full accumulated transcript piped into `INPUT.TXT`, the screen is
 * cleared, and the entire `OUTPUT.TXT` is redrawn inside a Rich-styled
 * "OUTPUT.TXT Replay" panel. The prompt is `input: ` (typer).
 */

const HEADER = 'InCollege Live Replay';
const SUBTITLE =
  'Executable: bin/main | Transcript: .live_session.input.txt | Inputs:';
const PANEL_TOP = '╭───────────────── OUTPUT.TXT Replay ─────────────────╮';
const PANEL_BOT = '╰─────────────────────────────────────────────────────╯';
const HELP_HINT =
  'Type :help for commands. Enter any other text to log one input line.';

/** Wrap a block of program output inside the blue-bordered panel. */
function panel(lines: readonly string[]): TerminalEvent[] {
  return [
    { type: 'print', text: PANEL_TOP },
    ...lines.map<TerminalEvent>((line) => ({
      type: 'print',
      text: `│ ${line}`,
    })),
    { type: 'print', text: PANEL_BOT },
  ];
}

function frame(inputCount: number, body: readonly string[]): TerminalEvent[] {
  return [
    { type: 'print', text: HEADER },
    { type: 'print', text: `${SUBTITLE} ${inputCount}` },
    ...panel(body),
    { type: 'print', text: HELP_HINT },
    { type: 'print', text: '' },
  ];
}

const BOOT_OUTPUT = [
  '========================================',
  '     WELCOME TO INCOLLEGE',
  '========================================',
  '',
  'Please select an option:',
  '1. Login with existing account',
  '2. Create new account',
  '3. Exit',
  'Enter choice (1-3):',
];

const AFTER_2 = [
  ...BOOT_OUTPUT,
  '2',
  '',
  '=== CREATE NEW ACCOUNT ===',
  'Enter username:',
];

const AFTER_ALICE = [
  ...AFTER_2,
  'alice',
  'Enter password (8-12 chars, 1 uppercase, 1 digit, 1 special character):',
];

export const liveCLIDemoScript: readonly TerminalEvent[] = [
  // Initial boot: the script is invoked, full transcript replays
  // (empty in this case), COBOL runs, OUTPUT.TXT is framed in a panel.
  { type: 'print', text: '$ python3 tests/live_cli.py bin/main' },
  { type: 'pause', ms: 600 },
  ...frame(0, BOOT_OUTPUT),
  { type: 'pause', ms: 1000 },

  // User types "2" at the input: prompt — append to transcript, rerun,
  // clear, redraw the panel with the updated OUTPUT.TXT.
  { type: 'type', text: '2', speed: 80 },
  { type: 'pause', ms: 400 },
  { type: 'clear' },
  ...frame(1, AFTER_2),
  { type: 'pause', ms: 1100 },

  // Next input — "alice". Same loop: append, rerun, clear, redraw.
  { type: 'type', text: 'alice', speed: 80 },
  { type: 'pause', ms: 400 },
  { type: 'clear' },
  ...frame(2, AFTER_ALICE),
  { type: 'pause', ms: 1100 },

  // :show renders a transcript table of every logged input line.
  { type: 'type', text: ':show', speed: 80 },
  { type: 'pause', ms: 300 },
  { type: 'print', text: '' },
  { type: 'print', text: '┏━━━┳━━━━━━━━━━━━━━━━━━━━━━━┓' },
  { type: 'print', text: '┃ # ┃ Input                 ┃' },
  { type: 'print', text: '┡━━━╇━━━━━━━━━━━━━━━━━━━━━━━┩' },
  { type: 'print', text: '│ 1 │ 2                     │' },
  { type: 'print', text: '│ 2 │ alice                 │' },
  { type: 'print', text: '└───┴───────────────────────┘' },
  { type: 'pause', ms: 1400 },

  // :quit ends the session — transcript stays on disk for next launch.
  { type: 'type', text: ':quit', speed: 80 },
  { type: 'print', text: '' },
  { type: 'print', text: 'Session ended.' },
  { type: 'pause', ms: 1500 },
];

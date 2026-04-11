import type { TerminalEvent } from './create-account';

export const liveCLIDemoScript: readonly TerminalEvent[] = [
  { type: 'print', text: '$ incollege-test run tests/epic3/' },
  { type: 'pause', ms: 600 },
  { type: 'print', text: '' },
  { type: 'print', text: 'Running 4 fixtures in tests/epic3/ ...' },
  { type: 'pause', ms: 300 },
  { type: 'print', text: '  ✓ search_found.in.txt           PASS  (0.42s)' },
  { type: 'pause', ms: 200 },
  { type: 'print', text: '  ✗ search_not_found.in.txt       FAIL  (0.38s)' },
  { type: 'pause', ms: 200 },
  { type: 'print', text: '  ✓ search_case.in.txt            PASS  (0.41s)' },
  { type: 'pause', ms: 200 },
  { type: 'print', text: '  ✓ search_empty.in.txt           PASS  (0.39s)' },
  { type: 'print', text: '' },
  { type: 'print', text: '3/4 passed · 1 failed' },
  { type: 'print', text: '' },
  { type: 'print', text: 'Entering live mode. Type :help for commands.' },
  { type: 'print', text: '' },
  { type: 'print', text: 'incollege-test> ' },
  { type: 'pause', ms: 800 },

  // :help
  { type: 'type', text: ':help', speed: 60 },
  { type: 'print', text: '' },
  { type: 'print', text: '  :help       Show this help' },
  { type: 'print', text: '  :show N     Show diff for fixture N' },
  { type: 'print', text: '  :dump N     Print raw actual output' },
  { type: 'print', text: '  :rerun N    Re-execute fixture N' },
  { type: 'print', text: '  :undo       Undo last edit' },
  { type: 'print', text: '  :quit       Exit live mode' },
  { type: 'print', text: '' },
  { type: 'print', text: 'incollege-test> ' },
  { type: 'pause', ms: 800 },

  // :show 2
  { type: 'type', text: ':show 2', speed: 60 },
  { type: 'print', text: '' },
  { type: 'print', text: '── search_not_found ──────────────────────' },
  { type: 'print', text: '- They are a part of the InCollege system.' },
  { type: 'print', text: '+ They are not yet a part of the InCollege system.' },
  { type: 'print', text: '' },
  { type: 'print', text: 'incollege-test> ' },
  { type: 'pause', ms: 800 },

  // :rerun 2
  { type: 'type', text: ':rerun 2', speed: 60 },
  { type: 'print', text: '' },
  { type: 'print', text: 'Re-running search_not_found.in.txt ...' },
  { type: 'pause', ms: 500 },
  { type: 'print', text: '  ✗ search_not_found.in.txt       FAIL  (0.37s)' },
  { type: 'print', text: '' },
  { type: 'print', text: 'incollege-test> ' },
  { type: 'pause', ms: 800 },

  // :quit
  { type: 'type', text: ':quit', speed: 60 },
  { type: 'print', text: '' },
  { type: 'print', text: 'Bye!' },
  { type: 'pause', ms: 1000 },
];

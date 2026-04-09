export type TerminalEvent =
  | { type: 'print'; text: string; delay?: number }
  | { type: 'type'; text: string; speed?: number }
  | { type: 'pause'; ms: number }
  | { type: 'clear' };

export const createAccountScript: readonly TerminalEvent[] = [
  { type: 'print', text: '========================================' },
  { type: 'print', text: '     WELCOME TO INCOLLEGE' },
  { type: 'print', text: '========================================' },
  { type: 'print', text: '' },
  { type: 'print', text: 'Please select an option:' },
  { type: 'print', text: '1. Login with existing account' },
  { type: 'print', text: '2. Create new account' },
  { type: 'print', text: '3. Exit' },
  { type: 'print', text: 'Enter choice (1-3):' },
  { type: 'pause', ms: 500 },
  { type: 'type', text: '2' },
  { type: 'print', text: '' },
  { type: 'print', text: '=== CREATE NEW ACCOUNT ===' },
  { type: 'print', text: 'Enter username:' },
  { type: 'pause', ms: 400 },
  { type: 'type', text: 'alice' },
  { type: 'print', text: 'alice' },
  {
    type: 'print',
    text:
      'Enter password (8-12 chars, 1 uppercase, 1 digit, 1 special character):',
  },
  { type: 'pause', ms: 400 },
  { type: 'type', text: 'short' },
  { type: 'print', text: '********' },
  { type: 'print', text: 'Password does not meet requirements.' },
  { type: 'print', text: 'Please try again.' },
  {
    type: 'print',
    text:
      'Enter password (8-12 chars, 1 uppercase, 1 digit, 1 special character):',
  },
  { type: 'pause', ms: 500 },
  { type: 'type', text: 'Alice1!1' },
  { type: 'print', text: '********' },
  { type: 'print', text: 'Account created successfully!' },
  { type: 'print', text: '' },
  { type: 'print', text: 'Please select an option:' },
  { type: 'print', text: '1. Login with existing account' },
  { type: 'print', text: '2. Create new account' },
  { type: 'print', text: '3. Exit' },
  { type: 'print', text: 'Enter choice (1-3):' },
  { type: 'pause', ms: 800 },
] as const;

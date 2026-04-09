export type TerminalEvent =
  | { type: 'print'; text: string; delay?: number }
  | { type: 'type'; text: string; speed?: number }
  | { type: 'pause'; ms: number }
  | { type: 'clear' };

export const findSomeoneScript: readonly TerminalEvent[] = [
  { type: 'print', text: '=== MAIN MENU ===' },
  { type: 'print', text: '1. Create/Edit My Profile' },
  { type: 'print', text: '2. View My Profile' },
  { type: 'print', text: '3. Search for a job' },
  { type: 'print', text: '4. Find someone you know' },
  { type: 'print', text: '5. View Pending Connection Requests' },
  { type: 'print', text: '6. Learn a new skill' },
  { type: 'print', text: '7. View My Network' },
  { type: 'print', text: '8. Messages' },
  { type: 'print', text: '9. Logout' },
  { type: 'print', text: 'Enter choice (1-9):' },
  { type: 'pause', ms: 400 },
  { type: 'type', text: '4' },
  { type: 'print', text: '' },
  { type: 'print', text: 'Enter the full name of the person you are looking for:' },
  { type: 'pause', ms: 400 },
  { type: 'type', text: 'Bob Jones' },
  { type: 'print', text: '--- Found User Profile ---' },
  { type: 'print', text: 'Name: Bob Jones' },
  { type: 'print', text: 'University: University of South Florida' },
  { type: 'print', text: 'Major: Information Technology' },
  { type: 'print', text: 'Graduation Year: 2027' },
  { type: 'print', text: 'About Me: Interested in cloud infrastructure.' },
  { type: 'print', text: 'Experience: None' },
  { type: 'print', text: 'Education: None' },
  { type: 'print', text: '-------------------------' },
  { type: 'print', text: '' },
  { type: 'print', text: '=== SEND CONNECTION REQUEST ===' },
  { type: 'print', text: '1. Send Connection Request' },
  { type: 'print', text: '2. Back to Main Menu' },
  { type: 'print', text: 'Enter your choice:' },
  { type: 'pause', ms: 800 },
] as const;

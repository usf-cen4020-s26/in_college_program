export type TerminalEvent =
  | { type: 'print'; text: string; delay?: number }
  | { type: 'type'; text: string; speed?: number }
  | { type: 'pause'; ms: number }
  | { type: 'clear' };

export const buildProfileScript: readonly TerminalEvent[] = [
  { type: 'print', text: 'You have successfully logged in' },
  { type: 'print', text: '' },
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
  { type: 'type', text: '1' },
  { type: 'print', text: '' },
  { type: 'print', text: '=== CREATE MY PROFILE ===' },
  { type: 'print', text: 'Enter First Name:' },
  { type: 'type', text: 'Alice' },
  { type: 'print', text: 'Enter Last Name:' },
  { type: 'type', text: 'Smith' },
  { type: 'print', text: 'Enter University/College Attended:' },
  { type: 'type', text: 'University of South Florida' },
  { type: 'print', text: 'Enter Major:' },
  { type: 'type', text: 'Computer Science' },
  { type: 'print', text: 'Enter Graduation Year (YYYY):' },
  { type: 'type', text: '2027' },
  {
    type: 'print',
    text:
      'Enter About Me (optional, max 200 chars, enter blank line to skip):',
  },
  { type: 'type', text: 'CS student interested in distributed systems.' },
  { type: 'print', text: '' },
  {
    type: 'print',
    text:
      "Add Experience (optional, max 3 entries. Enter anything to continue and 'DONE' to finish):",
  },
  { type: 'type', text: 'Software Intern' },
  { type: 'print', text: 'Enter Company Name:' },
  { type: 'type', text: 'Tesla' },
  { type: 'print', text: 'Enter Dates (e.g., Jun 2024 - Aug 2024):' },
  { type: 'type', text: 'May 2025 - Aug 2025' },
  { type: 'print', text: 'Enter Description (max 100 chars):' },
  { type: 'type', text: 'Worked on autopilot telemetry pipelines.' },
  {
    type: 'print',
    text:
      "Add Experience (optional, max 3 entries. Enter anything to continue and 'DONE' to finish):",
  },
  { type: 'type', text: 'DONE' },
  { type: 'print', text: '' },
  {
    type: 'print',
    text:
      "Add Education (optional, max 3 entries. Enter anything to continue and 'DONE' to finish):",
  },
  { type: 'type', text: 'DONE' },
  { type: 'print', text: 'Profile saved successfully!' },
  { type: 'pause', ms: 700 },
] as const;

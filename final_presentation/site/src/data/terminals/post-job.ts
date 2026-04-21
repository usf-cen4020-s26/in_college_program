export type TerminalEvent =
  | { type: 'print'; text: string; delay?: number }
  | { type: 'type'; text: string; speed?: number }
  | { type: 'pause'; ms: number }
  | { type: 'clear' };

export const postJobScript: readonly TerminalEvent[] = [
  { type: 'print', text: '=== MAIN MENU ===' },
  { type: 'print', text: '3. Search for a job' },
  { type: 'print', text: 'Enter choice (1-9):' },
  { type: 'pause', ms: 350 },
  { type: 'type', text: '3' },
  { type: 'print', text: '' },
  { type: 'print', text: '--- Job Search/Internship Menu ---' },
  { type: 'print', text: '1. Post a Job/Internship' },
  { type: 'print', text: '2. Browse Jobs/Internships' },
  { type: 'print', text: '3. View My Applications' },
  { type: 'print', text: '4. Back to Main Menu' },
  { type: 'print', text: 'Enter your choice (1-4):' },
  { type: 'pause', ms: 350 },
  { type: 'type', text: '1' },
  { type: 'print', text: '' },
  { type: 'print', text: '--- Post a New Job/Internship ---' },
  { type: 'print', text: '' },
  { type: 'print', text: 'Enter Job Title:' },
  { type: 'pause', ms: 400 },
  { type: 'type', text: '' },
  { type: 'print', text: 'Job Title is required. Please try again.' },
  { type: 'print', text: 'Enter Job Title:' },
  { type: 'pause', ms: 400 },
  { type: 'type', text: 'UX Designer' },
  { type: 'print', text: 'Enter Description (max 200 chars):' },
  { type: 'type', text: 'Design user flows and wireframes' },
  { type: 'print', text: 'Enter Employer Name:' },
  { type: 'type', text: 'DesignStudio' },
  { type: 'print', text: 'Enter Location:' },
  { type: 'type', text: 'Tampa, FL' },
  { type: 'print', text: "Enter Salary (optional, enter 'NONE' to skip):" },
  { type: 'type', text: '$45/hour' },
  { type: 'print', text: 'Job posted successfully!' },
  { type: 'print', text: '----------------------------------' },
  { type: 'pause', ms: 800 },
] as const;

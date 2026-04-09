export type TerminalEvent =
  | { type: 'print'; text: string; delay?: number }
  | { type: 'type'; text: string; speed?: number }
  | { type: 'pause'; ms: number }
  | { type: 'clear' };

export const myApplicationsScript: readonly TerminalEvent[] = [
  { type: 'print', text: '--- Job Search/Internship Menu ---' },
  { type: 'print', text: '1. Post a Job/Internship' },
  { type: 'print', text: '2. Browse Jobs/Internships' },
  { type: 'print', text: '3. View My Applications' },
  { type: 'print', text: '4. Back to Main Menu' },
  { type: 'print', text: 'Enter your choice (1-4):' },
  { type: 'pause', ms: 400 },
  { type: 'type', text: '3' },
  { type: 'print', text: '' },
  { type: 'print', text: '--- Your Job Applications ---' },
  { type: 'print', text: 'Application Summary for alice' },
  { type: 'print', text: '------------------------------' },
  { type: 'print', text: 'Job Title: Software Engineer Intern' },
  { type: 'print', text: 'Employer: Google' },
  { type: 'print', text: 'Location: Tampa, FL' },
  { type: 'print', text: '---' },
  { type: 'print', text: 'Job Title: UX Designer' },
  { type: 'print', text: 'Employer: DesignStudio' },
  { type: 'print', text: 'Location: Tampa, FL' },
  { type: 'print', text: '---' },
  { type: 'print', text: '------------------------------' },
  { type: 'print', text: 'Total Applications: 2' },
  { type: 'print', text: '------------------------------' },
  { type: 'pause', ms: 1000 },
] as const;

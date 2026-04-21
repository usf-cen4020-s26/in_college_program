export type TerminalEvent =
  | { type: 'print'; text: string; delay?: number }
  | { type: 'type'; text: string; speed?: number }
  | { type: 'pause'; ms: number }
  | { type: 'clear' };

export const browseApplyScript: readonly TerminalEvent[] = [
  { type: 'print', text: '--- Job Search/Internship Menu ---' },
  { type: 'print', text: '1. Post a Job/Internship' },
  { type: 'print', text: '2. Browse Jobs/Internships' },
  { type: 'print', text: '3. View My Applications' },
  { type: 'print', text: '4. Back to Main Menu' },
  { type: 'print', text: 'Enter your choice (1-4):' },
  { type: 'pause', ms: 400 },
  { type: 'type', text: '2' },
  { type: 'print', text: '' },
  { type: 'print', text: '--- Available Job Listings ---' },
  { type: 'print', text: '1. Software Engineer Intern at Google (Tampa, FL)' },
  { type: 'print', text: '-----------------------------' },
  { type: 'print', text: 'Enter job number to view details, or 0 to go back:' },
  { type: 'pause', ms: 400 },
  { type: 'type', text: '1' },
  { type: 'print', text: '' },
  { type: 'print', text: '--- Job Details ---' },
  { type: 'print', text: 'Title: Software Engineer Intern' },
  { type: 'print', text: 'Description: Build and test application features' },
  { type: 'print', text: 'Employer: Google' },
  { type: 'print', text: 'Location: Tampa, FL' },
  { type: 'print', text: 'Salary: $25/hr' },
  { type: 'print', text: '-------------------' },
  { type: 'print', text: '1. Apply for this Job' },
  { type: 'print', text: '2. Back to Job List' },
  { type: 'print', text: 'Enter your choice:' },
  { type: 'pause', ms: 400 },
  { type: 'type', text: '1' },
  {
    type: 'print',
    text:
      'Your application for Software Engineer Intern at Google has been submitted.',
  },
  { type: 'pause', ms: 800 },
] as const;

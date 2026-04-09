export type StorageStrategy = 'rewrite' | 'append' | 'hybrid';

export type StorageFile = {
  name: string;
  purpose: string;
  recordLayout: readonly string[];
  strategy: StorageStrategy;
  capacity: string;
};

export const STORAGE: readonly StorageFile[] = [
  {
    name: 'ACCOUNTS.DAT',
    purpose: 'Username and password for every registered user.',
    recordLayout: [
      'ACCT-USERNAME  PIC X(20)',
      'ACCT-PASSWORD  PIC X(12)',
    ],
    strategy: 'rewrite',
    capacity: 'Max 5 accounts',
  },
  {
    name: 'PROFILES.DAT',
    purpose:
      'Per-user profile record including name, school, major, grad year, about-me, up to 3 experiences, and up to 3 educations.',
    recordLayout: [
      'PROF-USERNAME     PIC X(20)',
      'PROF-HAS-PROFILE  PIC 9',
      'PROF-FIRST-NAME   PIC X(30)',
      'PROF-LAST-NAME    PIC X(30)',
      'PROF-UNIVERSITY   PIC X(50)',
      'PROF-MAJOR        PIC X(50)',
      'PROF-GRAD-YEAR    PIC X(4)',
      'PROF-ABOUT-ME     PIC X(200)',
      'PROF-EXPERIENCE   OCCURS 3 (title/company/dates/desc)',
      'PROF-EDUCATION    OCCURS 3 (degree/university/years)',
    ],
    strategy: 'rewrite',
    capacity: 'One record per account',
  },
  {
    name: 'PENDING.DAT',
    purpose:
      'Outstanding connection requests awaiting accept/reject from the recipient.',
    recordLayout: [
      'PEND-SENDER-USERNAME     PIC X(20)',
      'PEND-RECIPIENT-USERNAME  PIC X(20)',
      'PEND-STATUS              PIC X(1)',
    ],
    strategy: 'hybrid',
    capacity: 'Unbounded',
  },
  {
    name: 'CONNECTIONS.DAT',
    purpose:
      'Established undirected connections between two users — referenced for connection-gated messaging.',
    recordLayout: [
      'CONN-USER-A  PIC X(20)',
      'CONN-USER-B  PIC X(20)',
    ],
    strategy: 'append',
    capacity: 'Unbounded',
  },
  {
    name: 'JOBS.DAT',
    purpose:
      'Every job/internship posting, with poster, title, description, employer, location, and optional salary.',
    recordLayout: [
      'JOB-ID          PIC 9(5)',
      'JOB-POSTER      PIC X(20)',
      'JOB-TITLE       PIC X(50)',
      'JOB-DESCRIPTION PIC X(200)',
      'JOB-EMPLOYER    PIC X(50)',
      'JOB-LOCATION    PIC X(50)',
      'JOB-SALARY      PIC X(20)',
    ],
    strategy: 'append',
    capacity: 'Unbounded',
  },
  {
    name: 'APPLICATIONS.DAT',
    purpose:
      'Every job application submitted, used by View My Applications and the duplicate-apply guard.',
    recordLayout: [
      'APP-USERNAME     PIC X(20)',
      'APP-JOB-ID       PIC 9(5)',
      'APP-JOB-TITLE    PIC X(50)',
      'APP-JOB-EMPLOYER PIC X(50)',
      'APP-JOB-LOCATION PIC X(50)',
    ],
    strategy: 'append',
    capacity: 'Unbounded',
  },
  {
    name: 'MESSAGES.DAT',
    purpose:
      'Every message ever sent between connected users — the backing store behind Epic 9\'s inbox.',
    recordLayout: [
      'MSG-ID        PIC 9(5)',
      'MSG-SENDER    PIC X(20)',
      'MSG-RECIPIENT PIC X(20)',
      'MSG-CONTENT   PIC X(200)',
      'MSG-TIMESTAMP PIC X(20)',
    ],
    strategy: 'append',
    capacity: 'Unbounded',
  },
] as const;

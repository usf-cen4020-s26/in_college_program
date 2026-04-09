export const STATS = {
  epics: 9,
  modules: 25,
  loc: 2200,
  tests: 195,
  dataFiles: 7,
  testCategories: 9,
  multiPartTests: 70,
  viewMessageTests: 24,
} as const;

export type CategoryStat = {
  category: string;
  count: number;
  description: string;
};

export const TEST_CATEGORIES: readonly CategoryStat[] = [
  {
    category: 'profiles',
    count: 94,
    description:
      'Profile create/edit/view, connection requests, and cross-login persistence flows.',
  },
  {
    category: 'view_message',
    count: 24,
    description:
      'Epic 9 message inbox: recipient isolation, chronological order, and persistence.',
  },
  {
    category: 'job_internship_posting',
    count: 20,
    description:
      'Required-field re-prompts, optional salary, and multi-user posting sessions.',
  },
  {
    category: 'job_browsing',
    count: 18,
    description:
      'Browse, view details, apply, duplicate-application guards, and per-user application reports.',
  },
  {
    category: 'send_message',
    count: 15,
    description:
      'Connection-gated messaging, validation of recipient, and append-on-send persistence.',
  },
  {
    category: 'login',
    count: 13,
    description:
      'Account creation, password policy, account cap, and authentication happy paths.',
  },
  {
    category: 'main_menu',
    count: 5,
    description:
      'Find Someone You Know, skills, and post-login navigation.',
  },
  {
    category: 'eof_tests',
    count: 5,
    description:
      'Graceful shutdown when input is truncated in the middle of a flow.',
  },
  {
    category: 'seeding',
    count: 1,
    description:
      'Smoke test for the seed harness that primes DAT files before a fixture runs.',
  },
] as const;

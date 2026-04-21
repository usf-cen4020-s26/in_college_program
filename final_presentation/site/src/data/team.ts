export type TeamMember = {
  name: string;
  role: string;
  initials: string;
  contribution: string;
};

export const TEAM: readonly TeamMember[] = [
  {
    name: 'Trevor Flahardy',
    role: 'Scrum Master',
    initials: 'TF',
    contribution:
      'Coordinated the sprint, ran daily standups, and merged PRs #35 and #36 into main.',
  },
  {
    name: 'Aaron Fraze',
    role: 'Coder #1',
    initials: 'AF',
    contribution:
      'Implemented the 7840-VIEW-MESSAGES paragraph that powers the Epic 9 message inbox.',
  },
  {
    name: 'Olga Druzhkova',
    role: 'Coder #2',
    initials: 'OD',
    contribution:
      'Shipped display formatting (MSW-466/467) and audited docs so every story traced to code.',
  },
  {
    name: 'Melaine Fernandez Sarduy',
    role: 'Tester #1',
    initials: 'MF',
    contribution:
      'Built the seed harness that makes every fixture reproducible from a clean working tree.',
  },
  {
    name: 'Victoria Field',
    role: 'Tester #2',
    initials: 'VF',
    contribution:
      'Wrote the positive, negative, edge, and persistence fixtures that exercise the messaging flow.',
  },
] as const;

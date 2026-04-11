export type RoleRow = {
  epic: number;
  scrumMaster: string;
  coder1: string;
  coder2: string;
  tester1: string;
  tester2: string;
};

export const ROLE_ROTATION: readonly RoleRow[] = [
  { epic: 1, scrumMaster: 'Trevor', coder1: 'Aaron', coder2: 'Olga', tester1: 'Melaine', tester2: 'Victoria' },
  { epic: 2, scrumMaster: 'Melaine', coder1: 'Victoria', coder2: 'Trevor', tester1: 'Aaron', tester2: 'Olga' },
  { epic: 3, scrumMaster: 'Victoria', coder1: 'Trevor', coder2: 'Aaron', tester1: 'Olga', tester2: 'Melaine' },
  { epic: 4, scrumMaster: 'Trevor', coder1: 'Aaron', coder2: 'Olga', tester1: 'Melaine', tester2: 'Victoria' },
  { epic: 5, scrumMaster: 'Aaron', coder1: 'Olga', coder2: 'Melaine', tester1: 'Victoria', tester2: 'Trevor' },
  { epic: 6, scrumMaster: 'Olga', coder1: 'Melaine', coder2: 'Victoria', tester1: 'Trevor', tester2: 'Aaron' },
  { epic: 7, scrumMaster: 'Melaine', coder1: 'Victoria', coder2: 'Trevor', tester1: 'Aaron', tester2: 'Olga' },
  { epic: 8, scrumMaster: 'Victoria', coder1: 'Trevor', coder2: 'Aaron', tester1: 'Olga', tester2: 'Melaine' },
  { epic: 9, scrumMaster: 'All', coder1: 'All', coder2: 'All', tester1: 'All', tester2: 'All' },
];

import type { SlideEntry } from './types';

// Part A — Agile at a Glance
import { Slide01Title } from './01-title';
import { Slide02Team } from './02-team';
import { Slide03HowWeWork } from './03-how-we-work';
import { Slide04Rotation } from './04-rotation';
import { Slide05Branching } from './05-branching';
import { Slide06Numbers } from './06-by-the-numbers';

// Part B — Milestone 1: Authentication
import { Slide07M1Title } from './07-m1-title';
import { Slide08M1Features } from './08-m1-features';
import { Slide09M1Terminal } from './09-m1-terminal';
import { Slide10M1Password } from './10-m1-password';
import { Slide11M1Testing } from './11-m1-testing';
import { Slide12M1Architecture } from './12-m1-architecture';

// Part B — Milestone 2: User Profiles
import { Slide13M2Title } from './13-m2-title';
import { Slide14M2Features } from './14-m2-features';
import { Slide15M2Growth } from './15-m2-growth';

// Part B — Milestone 3: User Search
import { Slide16M3Title } from './16-m3-title';
import { Slide17M3Search } from './17-m3-search';

// Part B — Interlude A: The Test Runner
import { Slide18InterludeATitle } from './18-interlude-a-title';
import { Slide19TRBirth } from './19-tr-birth';
import { Slide20TRFileIO } from './20-tr-fileio';
import { Slide21TRComments } from './21-tr-comments';
import { Slide22TRLive } from './22-tr-live';
import { Slide22bTRLiveDemo } from './22b-tr-live-demo';

// Part B — Milestone 4: Connection Requests
import { Slide23M4Title } from './23-m4-title';
import { Slide24M4Connections } from './24-m4-connections';
import { Slide25M4MenuProblem } from './25-m4-menu-problem';

// Part B — Milestone 5: Accept/Reject & Network
import { Slide26M5Title } from './26-m5-title';
import { Slide27M5Bidirectional } from './27-m5-bidirectional';
import { Slide28M5Testing } from './28-m5-testing';

// Part B — Interlude B: Macros & Seeds
import { Slide29IntBTitle } from './29-interlude-b-title';
import { Slide30MacroProblem } from './30-macro-problem';
import { Slide31OutputMacros } from './31-output-macros';
import { Slide32SeedDirectives } from './32-seed-directives';
import { Slide33WhySeeds } from './33-why-seeds';
import { Slide34SeedTypes } from './34-seed-types';

// Part B — Milestone 6: Job Posting
import { Slide35M6Title } from './35-m6-title';
import { Slide36M6PostJob } from './36-m6-postjob';
import { Slide37M6Package } from './37-m6-package';

// Part B — Milestone 7: Browse & Apply
import { Slide38M7Title } from './38-m7-title';
import { Slide39M7Browse } from './39-m7-browse';
import { Slide40M7Progress } from './40-m7-progress';

// Interlude C — The Great Modularization
import { Slide41IntCTitle } from './41-interlude-c-title';
import { Slide42ModularBA } from './42-modular-before-after';
import { Slide43CopybookMap } from './43-copybook-map';
import { Slide44Paragraphs } from './44-paragraph-numbering';

// Part B — Milestone 8: Send Messages
import { Slide45M8Title } from './45-m8-title';
import { Slide46M8Flow } from './46-m8-flow';
import { Slide47M8Data } from './47-m8-data';

// Part C — Epic 9 Deep Dive
import { Slide48Epic9Title } from './48-epic9-title';
import { Slide49Story1 } from './49-epic9-story1';
import { Slide50Story2 } from './50-epic9-story2';
import { Slide51Story3 } from './51-epic9-story3';
import { Slide52Story4 } from './52-epic9-story4';
import { Slide53Terminal } from './53-epic9-terminal';
import { Slide54Testing } from './54-epic9-testing';

// Part D — Architecture & Design Patterns
import { Slide55DataArch } from './55-data-arch';
import { Slide56ProgramFlow } from './56-program-flow';
import { Slide57Security } from './57-security';
import { Slide58Pipeline } from './58-test-pipeline';
import { Slide59Patterns } from './59-cobol-patterns';
import { Slide60FinalStats } from './60-final-stats';

// Part E — Closing
import { Slide61Journey } from './61-journey';
import { Slide62WhatsNext } from './62-whats-next';
import { Slide63Close } from './63-close';

export const SLIDES: readonly SlideEntry[] = [
  // ── Part A · Agile at a Glance ──────────────────────────────────────
  { id: '01-title', title: 'InCollege', act: 'PART A · AGILE AT A GLANCE', speaker: 'Trevor', component: Slide01Title },
  { id: '02-team', title: 'Meet the Team', act: 'PART A · AGILE AT A GLANCE', speaker: 'Trevor', component: Slide02Team },
  { id: '03-how-we-work', title: 'How We Work', act: 'PART A · AGILE AT A GLANCE', speaker: 'Trevor', component: Slide03HowWeWork, steps: 3 },
  { id: '04-rotation', title: 'Rotating Roles', act: 'PART A · AGILE AT A GLANCE', speaker: 'Trevor', component: Slide04Rotation, steps: 2 },
  { id: '05-branching', title: 'Branching Strategy', act: 'PART A · AGILE AT A GLANCE', speaker: 'Trevor', component: Slide05Branching, steps: 2 },
  { id: '06-numbers', title: 'By the Numbers', act: 'PART A · AGILE AT A GLANCE', speaker: 'Trevor', component: Slide06Numbers },

  // ── Part B · Milestone 1: Authentication ─────────────────────────────
  { id: '07-m1-title', title: 'Authentication', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Aaron', component: Slide07M1Title },
  { id: '08-m1-features', title: 'What We Built', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Aaron', component: Slide08M1Features, steps: 2 },
  { id: '09-m1-terminal', title: 'Create an Account', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Aaron', component: Slide09M1Terminal },
  { id: '10-m1-password', title: 'Password Validation', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Aaron', component: Slide10M1Password, steps: 2 },
  { id: '11-m1-testing', title: 'Testing Begins', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Aaron', component: Slide11M1Testing, steps: 2 },
  { id: '12-m1-arch', title: 'Architecture: Week 1', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Aaron', component: Slide12M1Architecture },

  // ── Part B · Milestone 2: User Profiles ──────────────────────────────
  { id: '13-m2-title', title: 'User Profiles', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Aaron', component: Slide13M2Title },
  { id: '14-m2-features', title: 'Build Your Profile', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Aaron', component: Slide14M2Features, steps: 2 },
  { id: '15-m2-growth', title: 'Growing Pains', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Aaron', component: Slide15M2Growth },

  // ── Part B · Milestone 3: User Search ────────────────────────────────
  { id: '16-m3-title', title: 'User Search', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Olga', component: Slide16M3Title },
  { id: '17-m3-search', title: 'Find Someone', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Olga', component: Slide17M3Search },

  // ── Interlude A · The Test Runner ────────────────────────────────────
  { id: '18-int-a', title: 'The Test Runner', act: 'INTERLUDE', speaker: 'Trevor', component: Slide18InterludeATitle, breakout: true },
  { id: '19-tr-birth', title: 'Phase 1: Birth', act: 'INTERLUDE', speaker: 'Trevor', component: Slide19TRBirth, steps: 2 },
  { id: '20-tr-fileio', title: 'Phase 2: File I/O', act: 'INTERLUDE', speaker: 'Trevor', component: Slide20TRFileIO, steps: 2 },
  { id: '21-tr-comments', title: 'Comments & Debug', act: 'INTERLUDE', speaker: 'Trevor', component: Slide21TRComments, steps: 2 },
  { id: '22-tr-live', title: 'Live CLI & Packaging', act: 'INTERLUDE', speaker: 'Trevor', component: Slide22TRLive, steps: 2 },
  { id: '22b-tr-demo', title: 'Live Mode Demo', act: 'INTERLUDE', speaker: 'Trevor', component: Slide22bTRLiveDemo },

  // ── Part B · Milestone 4: Connection Requests ────────────────────────
  { id: '23-m4-title', title: 'Connection Requests', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Olga', component: Slide23M4Title },
  { id: '24-m4-connections', title: 'Send & View', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Olga', component: Slide24M4Connections, steps: 2 },
  { id: '25-m4-menu', title: 'The Menu Problem', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Olga', component: Slide25M4MenuProblem },

  // ── Part B · Milestone 5: Accept/Reject & Network ────────────────────
  { id: '26-m5-title', title: 'Network', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Olga', component: Slide26M5Title },
  { id: '27-m5-bidir', title: 'Bidirectional', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Olga', component: Slide27M5Bidirectional, steps: 2 },
  { id: '28-m5-testing', title: 'Testing at Scale', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Olga', component: Slide28M5Testing },

  // ── Interlude B · Macros & Seeds ─────────────────────────────────────
  { id: '29-int-b', title: 'Macros & Seeds', act: 'INTERLUDE', speaker: 'Victoria', component: Slide29IntBTitle, breakout: true },
  { id: '30-macro-problem', title: 'The Problem', act: 'INTERLUDE', speaker: 'Victoria', component: Slide30MacroProblem, steps: 2 },
  { id: '31-output-macros', title: 'Output Macros', act: 'INTERLUDE', speaker: 'Victoria', component: Slide31OutputMacros, steps: 2 },
  { id: '32-seed-directives', title: 'Seed Directives', act: 'INTERLUDE', speaker: 'Victoria', component: Slide32SeedDirectives, steps: 2 },
  { id: '33-why-seeds', title: 'Why Seeds Matter', act: 'INTERLUDE', speaker: 'Victoria', component: Slide33WhySeeds },
  { id: '34-seed-types', title: 'Seed Types', act: 'INTERLUDE', speaker: 'Victoria', component: Slide34SeedTypes, steps: 2 },

  // ── Part B · Milestone 6: Job Posting ────────────────────────────────
  { id: '35-m6-title', title: 'Job Posting', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Victoria', component: Slide35M6Title },
  { id: '36-m6-postjob', title: 'Post a Job', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Victoria', component: Slide36M6PostJob },
  { id: '37-m6-package', title: 'Test Package', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Victoria', component: Slide37M6Package },

  // ── Part B · Milestone 7: Browse & Apply ─────────────────────────────
  { id: '38-m7-title', title: 'Browse & Apply', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Victoria', component: Slide38M7Title },
  { id: '39-m7-browse', title: 'Browse, Apply, Report', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Victoria', component: Slide39M7Browse, steps: 2 },
  { id: '40-m7-progress', title: 'Running Totals', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Victoria', component: Slide40M7Progress },

  // ── Interlude C · The Great Modularization ───────────────────────────
  { id: '41-int-c', title: 'Modularization', act: 'INTERLUDE', speaker: 'Trevor', component: Slide41IntCTitle, breakout: true },
  { id: '42-modular-ba', title: 'Before vs After', act: 'INTERLUDE', speaker: 'Trevor', component: Slide42ModularBA, steps: 2 },
  { id: '43-copybook-map', title: 'Copybook Map', act: 'INTERLUDE', speaker: 'Trevor', component: Slide43CopybookMap },
  { id: '44-paragraphs', title: 'Paragraph Numbering', act: 'INTERLUDE', speaker: 'Trevor', component: Slide44Paragraphs },

  // ── Part B · Milestone 8: Send Messages ──────────────────────────────
  { id: '45-m8-title', title: 'Send Messages', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Melaine', component: Slide45M8Title },
  { id: '46-m8-flow', title: 'Message Flow', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Melaine', component: Slide46M8Flow, steps: 2 },
  { id: '47-m8-data', title: 'MESSAGES.DAT', act: 'PART B · BUILDING INCOLLEGE', speaker: 'Melaine', component: Slide47M8Data },

  // ── Part C · Epic 9 Deep Dive ────────────────────────────────────────
  { id: '48-epic9-title', title: 'View Messages', act: 'PART C · EPIC 9 DEEP DIVE', speaker: 'Melaine', component: Slide48Epic9Title, breakout: true },
  { id: '49-story1', title: 'Basic View', act: 'PART C · EPIC 9 DEEP DIVE', speaker: 'Melaine', component: Slide49Story1, steps: 2 },
  { id: '50-story2', title: 'Ordering', act: 'PART C · EPIC 9 DEEP DIVE', speaker: 'Melaine', component: Slide50Story2, steps: 2 },
  { id: '51-story3', title: 'Isolation', act: 'PART C · EPIC 9 DEEP DIVE', speaker: 'Melaine', component: Slide51Story3 },
  { id: '52-story4', title: 'Persistence', act: 'PART C · EPIC 9 DEEP DIVE', speaker: 'Melaine', component: Slide52Story4 },
  { id: '53-terminal', title: 'Full Messaging Arc', act: 'PART C · EPIC 9 DEEP DIVE', speaker: 'Melaine', component: Slide53Terminal },
  { id: '54-testing', title: 'Epic 9 Testing', act: 'PART C · EPIC 9 DEEP DIVE', speaker: 'Melaine', component: Slide54Testing },

  // ── Part D · Architecture & Design Patterns ──────────────────────────
  { id: '55-data-arch', title: 'Data Architecture', act: 'PART D · ARCHITECTURE', speaker: 'Olga', component: Slide55DataArch, steps: 2 },
  { id: '56-program-flow', title: 'Program Flow', act: 'PART D · ARCHITECTURE', speaker: 'Trevor', component: Slide56ProgramFlow, steps: 2 },
  { id: '57-security', title: 'Security', act: 'PART D · ARCHITECTURE', speaker: 'Trevor', component: Slide57Security, steps: 2 },
  { id: '58-pipeline', title: 'Testing Pipeline', act: 'PART D · ARCHITECTURE', speaker: 'Trevor', component: Slide58Pipeline, steps: 2 },
  { id: '59-patterns', title: 'COBOL Patterns', act: 'PART D · ARCHITECTURE', speaker: 'Trevor', component: Slide59Patterns, steps: 4 },
  { id: '60-stats', title: 'Final Statistics', act: 'PART D · ARCHITECTURE', speaker: 'Trevor', component: Slide60FinalStats },

  // ── Part E · Closing ─────────────────────────────────────────────────
  { id: '61-journey', title: 'The Journey', act: 'PART E · CLOSING', speaker: 'Trevor', component: Slide61Journey },
  { id: '62-whats-next', title: "What's Next", act: 'PART E · CLOSING', speaker: 'All', component: Slide62WhatsNext },
  { id: '63-close', title: 'Thank You & Q&A', act: 'PART E · CLOSING', speaker: 'Trevor', component: Slide63Close },
];

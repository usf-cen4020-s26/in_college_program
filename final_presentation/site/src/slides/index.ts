import type { SlideEntry } from './types';

// Act I — The Pitch
import { Slide01Title } from './01-title';
import { Slide02Team } from './02-team';
import { Slide03Product } from './03-product';
import { Slide04ByTheNumbers } from './04-by-the-numbers';

// Act II — Product Demo
import { Slide05CreateAccount } from './05-create-account';
import { Slide06BuildProfile } from './06-build-profile';
import { Slide07FindSomeone } from './07-find-someone';
import { Slide08ConnectAcceptNetwork } from './08-connect-accept-network';
import { Slide09PostJob } from './09-post-job';
import { Slide10BrowseApply } from './10-browse-apply';
import { Slide11MyApplications } from './11-my-applications';
import { Slide12MessagingArc } from './12-messaging-arc';

// Act III — How We Built It
import { Slide13Architecture } from './13-architecture';
import { Slide14CopybookDeepDive } from './14-copybook-deep-dive';
import { Slide15DataStorage } from './15-data-storage';
import { Slide16CodeGemViewLoop } from './16-code-gem-view-loop';
import { Slide17CodeGemBidirectional } from './17-code-gem-bidirectional';
import { Slide18Security } from './18-security';
import { Slide19TestingAtScale } from './19-testing-at-scale';
import { Slide20HarnessFlow } from './20-harness-flow';

// Act IV — Team & Process
import { Slide21SprintTimeline } from './21-sprint-timeline';
import { Slide22BurndownStart } from './22-burndown-start';
import { Slide23BurndownEnd } from './23-burndown-end';

// Act V — Close
import { Slide24WhatsNext } from './24-whats-next';
import { Slide25Close } from './25-close';

export const SLIDES: readonly SlideEntry[] = [
  { id: '01-title', title: 'InCollege', act: 'ACT I · THE PITCH', speaker: 'Trevor', component: Slide01Title },
  { id: '02-team', title: 'Meet the Team', act: 'ACT I · THE PITCH', speaker: 'Trevor', component: Slide02Team },
  { id: '03-product', title: 'The Product', act: 'ACT I · THE PITCH', speaker: 'Trevor', component: Slide03Product },
  { id: '04-numbers', title: 'By the Numbers', act: 'ACT I · THE PITCH', speaker: 'Trevor', component: Slide04ByTheNumbers },

  { id: '05-create-account', title: 'Create Your Account', act: 'ACT II · PRODUCT DEMO', speaker: 'Aaron', component: Slide05CreateAccount },
  { id: '06-build-profile', title: 'Build Your Profile', act: 'ACT II · PRODUCT DEMO', speaker: 'Aaron', component: Slide06BuildProfile },
  { id: '07-find-someone', title: 'Find Someone You Know', act: 'ACT II · PRODUCT DEMO', speaker: 'Olga', component: Slide07FindSomeone },
  { id: '08-connect', title: 'Connect → Accept → Network', act: 'ACT II · PRODUCT DEMO', speaker: 'Olga', component: Slide08ConnectAcceptNetwork },
  { id: '09-post-job', title: 'Post a Job', act: 'ACT II · PRODUCT DEMO', speaker: 'Victoria', component: Slide09PostJob },
  { id: '10-browse-apply', title: 'Browse & Apply', act: 'ACT II · PRODUCT DEMO', speaker: 'Victoria', component: Slide10BrowseApply },
  { id: '11-my-applications', title: 'My Applications', act: 'ACT II · PRODUCT DEMO', speaker: 'Victoria', component: Slide11MyApplications },
  { id: '12-messaging', title: 'Messaging: The Full Arc', act: 'ACT II · PRODUCT DEMO', speaker: 'Melaine', component: Slide12MessagingArc },

  { id: '13-architecture', title: 'Architecture: 25 Modules', act: 'ACT III · HOW WE BUILT IT', speaker: 'Trevor', component: Slide13Architecture },
  { id: '14-copybooks', title: 'Copybook Deep Dive', act: 'ACT III · HOW WE BUILT IT', speaker: 'Trevor', component: Slide14CopybookDeepDive },
  { id: '15-storage', title: 'Data Storage', act: 'ACT III · HOW WE BUILT IT', speaker: 'Olga', component: Slide15DataStorage },
  { id: '16-code-gem-1', title: 'Code Gem · Recursive Message Read', act: 'ACT III · HOW WE BUILT IT', speaker: 'Olga', component: Slide16CodeGemViewLoop },
  { id: '17-code-gem-2', title: 'Code Gem · Bidirectional Connection', act: 'ACT III · HOW WE BUILT IT', speaker: 'Olga', component: Slide17CodeGemBidirectional },
  { id: '18-security', title: 'Security Features', act: 'ACT III · HOW WE BUILT IT', speaker: 'Trevor', component: Slide18Security },
  { id: '19-testing', title: 'Testing at Scale', act: 'ACT III · HOW WE BUILT IT', speaker: 'Trevor', component: Slide19TestingAtScale },
  { id: '20-harness', title: 'How the Harness Works', act: 'ACT III · HOW WE BUILT IT', speaker: 'Trevor', component: Slide20HarnessFlow },

  { id: '21-sprint', title: 'How We Ran the Sprint', act: 'ACT IV · TEAM & PROCESS', speaker: 'Trevor', component: Slide21SprintTimeline },
  { id: '22-burndown-start', title: 'Burndown · Sprint Start', act: 'ACT IV · TEAM & PROCESS', speaker: 'Trevor', component: Slide22BurndownStart },
  { id: '23-burndown-end', title: 'Burndown · Sprint End', act: 'ACT IV · TEAM & PROCESS', speaker: 'Trevor', component: Slide23BurndownEnd },

  { id: '24-whats-next', title: "What's Next", act: 'ACT V · CLOSE', speaker: 'All', component: Slide24WhatsNext },
  { id: '25-close', title: 'Thank You · Q&A', act: 'ACT V · CLOSE', speaker: 'Trevor', component: Slide25Close },
];

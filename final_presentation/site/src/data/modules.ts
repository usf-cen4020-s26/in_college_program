export type Module = {
  name: string;
  description: string;
};

export type ModuleCluster = {
  name: string;
  color: string;
  modules: readonly Module[];
};

export const MODULE_CLUSTERS: readonly ModuleCluster[] = [
  {
    name: 'Core',
    color: '#0A66C2',
    modules: [
      {
        name: 'main.cob',
        description:
          'Entry point: FILE-CONTROL, FILE SECTION, working-storage COPY directives, and the main dispatch loop.',
      },
      {
        name: 'DATALOAD.cpy',
        description:
          'Startup routine that loads every DAT file into the in-memory tables before the banner is drawn.',
      },
    ],
  },
  {
    name: 'Authentication',
    color: '#38BDF8',
    modules: [
      {
        name: 'AUTH.cpy',
        description:
          'Login, account creation, password policy enforcement, and the 5-account cap.',
      },
    ],
  },
  {
    name: 'Profiles & Discovery',
    color: '#70B5F9',
    modules: [
      {
        name: 'PROFILE.cpy',
        description:
          'Create/edit/view a profile with required fields, about-me, experience, and education.',
      },
      {
        name: 'SEARCH.cpy',
        description:
          'Find Someone You Know — exact full-name match against the profiles table.',
      },
      {
        name: 'SKILLS.cpy',
        description:
          'Learn-a-new-skill menu with placeholder lessons gated by the logged-in user.',
      },
    ],
  },
  {
    name: 'Connections',
    color: '#22D3EE',
    modules: [
      {
        name: 'SENDREQ.cpy',
        description:
          'Send a connection request and write it to PENDING.DAT.',
      },
      {
        name: 'VIEWREQ.cpy',
        description:
          'View, accept, or reject pending connection requests for the current user.',
      },
      {
        name: 'CONNMGMT.cpy',
        description:
          'Top-level connection management menu that routes into send/view/network flows.',
      },
      {
        name: 'CONNWRITE.cpy',
        description:
          'Append established connections to CONNECTIONS.DAT and refresh the in-memory table.',
      },
      {
        name: 'NETWORK.cpy',
        description:
          'View My Network — lists every user connected to the logged-in account.',
      },
    ],
  },
  {
    name: 'Jobs',
    color: '#F59E0B',
    modules: [
      {
        name: 'JOBS.cpy',
        description:
          'Post a job or internship with required-field re-prompts and optional salary.',
      },
      {
        name: 'BROWSEJOBS.cpy',
        description:
          'Browse available jobs, drill into details, and route into the apply flow.',
      },
      {
        name: 'APPLYJOB.cpy',
        description:
          'Apply to a job, guarded by the duplicate-application check in 5327.',
      },
      {
        name: 'VIEWAPPS.cpy',
        description:
          'View My Applications — filtered per-user summary read from APPLICATIONS.DAT.',
      },
      {
        name: 'JOBSIO.cpy',
        description:
          'Shared JOBS.DAT read/write helpers reused by post, browse, and apply.',
      },
    ],
  },
  {
    name: 'Messaging',
    color: '#A855F7',
    modules: [
      {
        name: 'SENDMESSAGE.cpy',
        description:
          'Messages menu, send-message flow, and the 7820 bidirectional connection check.',
      },
      {
        name: 'VIEWMESSAGE.cpy',
        description:
          'View-my-messages recursive loop with recipient isolation on every record.',
      },
    ],
  },
  {
    name: 'Working Storage',
    color: '#94A3B8',
    modules: [
      {
        name: 'WS-CONSTANTS.cpy',
        description:
          'File status codes, account limits, and app-wide magic numbers in one place.',
      },
      {
        name: 'WS-ACCOUNTS.cpy',
        description:
          'In-memory accounts table plus the current-user index used across every paragraph.',
      },
      {
        name: 'WS-PROFILES.cpy',
        description:
          'In-memory profiles table mirroring PROFILES.DAT for fast reads.',
      },
      {
        name: 'WS-CONNECTIONS.cpy',
        description:
          'In-memory connections table consulted on every send-message call.',
      },
      {
        name: 'WS-JOBS.cpy',
        description:
          'In-memory jobs and applications state, counters, and selected-job index.',
      },
      {
        name: 'WS-MESSAGES.cpy',
        description:
          'Messaging state: EOF flags, found counters, and recipient validation variables.',
      },
    ],
  },
  {
    name: 'I/O Control',
    color: '#EF4444',
    modules: [
      {
        name: 'WS-IO-CONTROL.cpy',
        description:
          'Input/output buffers, EOF flag, and the WS-OUTPUT-LINE variable every paragraph writes through.',
      },
    ],
  },
] as const;

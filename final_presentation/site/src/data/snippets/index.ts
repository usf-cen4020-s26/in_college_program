import viewLoop from './view-loop.cob?raw';
import bidirectional from './bidirectional.cob?raw';
import passwordPolicy from './password-policy.cob?raw';

export const SNIPPETS = {
  viewLoop,
  bidirectional,
  passwordPolicy,
} as const;

export type SnippetKey = keyof typeof SNIPPETS;

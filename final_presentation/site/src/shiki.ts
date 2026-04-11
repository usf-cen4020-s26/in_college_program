import { createHighlighter, type Highlighter } from 'shiki';

/**
 * Singleton Shiki highlighter preloaded with the COBOL grammar and a
 * custom theme aligned to our palette. All code panels share this one
 * instance so grammar/themes only load once.
 *
 * Shiki bundles a COBOL TextMate grammar via shikijs/textmate-grammars-themes,
 * so `lang="cobol"` works out of the box.
 */

let instance: Highlighter | null = null;
let loading: Promise<Highlighter> | null = null;

const inCollegeTheme = {
  name: 'in-college-dark',
  type: 'dark',
  colors: {
    'editor.background': '#0B1222',
    'editor.foreground': '#F5F7FA',
  },
  tokenColors: [
    {
      scope: ['comment', 'punctuation.definition.comment'],
      settings: { foreground: '#5F6B82', fontStyle: 'italic' },
    },
    {
      scope: ['keyword', 'storage.type', 'storage.modifier'],
      settings: { foreground: '#70B5F9' },
    },
    {
      scope: ['string', 'string.quoted'],
      settings: { foreground: '#22C55E' },
    },
    {
      scope: ['constant.numeric', 'constant.language'],
      settings: { foreground: '#F59E0B' },
    },
    {
      scope: ['entity.name.function', 'support.function', 'meta.function-call'],
      settings: { foreground: '#38BDF8' },
    },
    {
      scope: ['variable', 'variable.other', 'meta.variable'],
      settings: { foreground: '#F5F7FA' },
    },
    {
      scope: ['entity.name.section', 'entity.name.tag'],
      settings: { foreground: '#70B5F9', fontStyle: 'bold' },
    },
    {
      scope: ['markup.inserted', 'punctuation.definition.inserted'],
      settings: { foreground: '#22C55E' },
    },
    {
      scope: ['markup.deleted', 'punctuation.definition.deleted'],
      settings: { foreground: '#EF4444' },
    },
  ],
} as const;

export async function getHighlighter(): Promise<Highlighter> {
  if (instance) return instance;
  if (loading) return loading;

  loading = createHighlighter({
    themes: [inCollegeTheme as unknown as Parameters<typeof createHighlighter>[0]['themes'][number]],
    langs: ['cobol', 'diff', 'bash'],
  }).then((hl) => {
    instance = hl;
    return hl;
  });

  return loading;
}

/**
 * Synchronous helper: returns highlighted HTML if the highlighter is
 * ready, or null otherwise. Slide components use this through a
 * useShikiHtml hook that re-renders once the highlighter loads.
 */
export function highlightSync(code: string): string | null {
  if (!instance) return null;
  return instance.codeToHtml(code, {
    lang: 'cobol',
    theme: 'in-college-dark',
  });
}

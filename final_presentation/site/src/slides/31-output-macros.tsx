import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { BeforeAfter } from '../components/BeforeAfter';

const RAW_MENU = `1. Create/Edit My Profile
2. View My Profile
3. Search for a job
4. Find someone you know
5. View Pending Connection Requests
6. Learn a New Skill
7. View My Network
8. Messages
9. Logout
Enter your choice:`;

const MACRO_MENU = `{{MAIN_MENU}}`;

const mono = { fontFamily: 'var(--font-mono)', fontSize: 'clamp(13px, 1em, 16px)', lineHeight: 1.7, color: 'var(--color-text-primary)', whiteSpace: 'pre-wrap' as const };

/**
 * Slide 31 — Output Macros: replace 12 raw lines with 1 macro.
 */
export function Slide31OutputMacros({ step }: SlideProps) {
  return (
    <SlideFrame act="INTERLUDE" kicker="SOLUTION #1 · OUTPUT MACROS">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-mono)',
          fontSize: 'clamp(36px, 4.5em, 72px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-brand-accent)',
          lineHeight: 1.05,
        }}
      >
        {'{{MAIN_MENU}}'}
      </h1>

      <BeforeAfter
        step={step}
        beforeLabel="Raw .out.txt · 12 lines"
        afterLabel="With Macros · 1 line"
        before={<pre style={mono}>{RAW_MENU}</pre>}
        after={
          <div style={{ display: 'flex', alignItems: 'center', justifyContent: 'center', flex: 1 }}>
            <pre style={{ ...mono, fontSize: 'clamp(20px, 2em, 36px)', color: 'var(--color-brand-accent)' }}>
              {MACRO_MENU}
            </pre>
          </div>
        }
      />

      <p
        style={{
          margin: 0,
          fontFamily: 'var(--font-body)',
          fontSize: 'clamp(14px, 1.1em, 18px)',
          color: 'var(--color-text-muted)',
          textAlign: 'center',
        }}
      >
        30+ macros defined in{' '}
        <span style={{ fontFamily: 'var(--font-mono)', color: 'var(--color-brand-accent)' }}>
          menus.yml
        </span>{' '}
        — one file to update.
      </p>
    </SlideFrame>
  );
}

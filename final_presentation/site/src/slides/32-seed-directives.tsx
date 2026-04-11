import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { BeforeAfter } from '../components/BeforeAfter';

const MANUAL = `2
alice
Alice1!
Alice
Smith
USF
CS
2027

9
2
bob
Bob123!
Bob
Jones
USF
IT
2026

9
1
alice
Alice1!`;

const SEED = `@seed_user username=alice password=Alice1! \\
  first_name=Alice last_name=Smith
@seed_user username=bob password=Bob123! \\
  first_name=Bob last_name=Jones
@seed_connection user_a=alice user_b=bob`;

const mono = { fontFamily: 'var(--font-mono)', fontSize: 'clamp(12px, 0.9em, 15px)', lineHeight: 1.65, color: 'var(--color-text-primary)', whiteSpace: 'pre-wrap' as const };

/**
 * Slide 32 — Seed Directives: from 30 lines to 3.
 */
export function Slide32SeedDirectives({ step }: SlideProps) {
  return (
    <SlideFrame act="INTERLUDE" kicker="SOLUTION #2 · SEED DIRECTIVES">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(40px, 5em, 80px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
          lineHeight: 1.05,
        }}
      >
        From 30 lines to <span className="text-gradient">3</span>
      </h1>

      <BeforeAfter
        step={step}
        beforeLabel="Manual Setup · 30+ lines"
        afterLabel="Seed Directives · 3 lines"
        before={<pre style={mono}>{MANUAL}</pre>}
        after={<pre style={{ ...mono, color: 'var(--color-brand-accent)' }}>{SEED}</pre>}
      />
    </SlideFrame>
  );
}

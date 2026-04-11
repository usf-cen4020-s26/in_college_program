import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';
import { CodePanel } from '../components/CodePanel';

/**
 * Slide 34 — Three seed directive types, revealed in steps.
 */
export function Slide34SeedTypes({ step }: SlideProps) {
  return (
    <SlideFrame act="INTERLUDE" kicker="SEED DIRECTIVES · TYPES">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(36px, 4.5em, 72px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
          lineHeight: 1.05,
        }}
      >
        Three <span className="text-gradient">Seed Directives</span>
      </h1>

      <div style={{ display: 'flex', flexDirection: 'column', gap: 'clamp(12px, 1em, 20px)', flex: 1, minHeight: 0 }}>
        <CodePanel
          filePath="@seed_user"
          code={`@seed_user username=alice password=Alice1! first_name=Alice last_name=Smith university=USF major=CS grad_year=2027`}
          lang="bash"
        />

        <StepReveal currentStep={step} visibleAt={1}>
          <div style={{ display: 'flex', flexDirection: 'column', gap: 'clamp(12px, 1em, 20px)' }}>
            <CodePanel
              filePath="@seed_connection"
              code={`@seed_connection user_a=alice user_b=bob`}
              lang="bash"
            />
            <CodePanel
              filePath="@seed_message"
              code={`@seed_message sender=alice recipient=bob content="Hello Bob!" timestamp="2026-04-10 14:30:00"`}
              lang="bash"
            />
          </div>
        </StepReveal>
      </div>
    </SlideFrame>
  );
}

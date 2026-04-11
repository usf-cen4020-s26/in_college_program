import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';

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
          fontSize: 'clamp(28px, 3.5em, 56px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
          flexShrink: 0,
        }}
      >
        Three <span className="text-gradient">Seed Directives</span>
      </h1>

      <div style={{ display: 'flex', flexDirection: 'column', gap: 'clamp(8px, 0.75em, 14px)', flex: 1, minHeight: 0 }}>
        {/* @seed_user */}
        <SeedCard
          name="@seed_user"
          color="#38BDF8"
          code={`@seed_user username=alice password=Alice1! \\
    first_name=Alice last_name=Smith \\
    university=USF major=CS grad_year=2027`}
        />

        {/* @seed_connection + @seed_message */}
        <StepReveal currentStep={step} visibleAt={1}>
          <div style={{ display: 'flex', flexDirection: 'column', gap: 'clamp(8px, 0.75em, 14px)' }}>
            <SeedCard
              name="@seed_connection"
              color="#22C55E"
              code="@seed_connection user_a=alice user_b=bob"
            />
            <SeedCard
              name="@seed_message"
              color="#F59E0B"
              code={`@seed_message sender=alice recipient=bob \\
    content="Hello Bob!" \\
    timestamp="2026-04-10 14:30:00"`}
            />
          </div>
        </StepReveal>
      </div>
    </SlideFrame>
  );
}

function SeedCard({ name, color, code }: { name: string; color: string; code: string }) {
  return (
    <div
      style={{
        borderRadius: 12,
        background: 'var(--color-bg-code)',
        border: `1px solid ${color}22`,
        borderLeft: `3px solid ${color}`,
        overflow: 'hidden',
      }}
    >
      <div
        style={{
          padding: 'clamp(4px, 0.4em, 8px) clamp(8px, 1em, 16px)',
          borderBottom: '1px solid var(--color-bg-code-border)',
          fontFamily: 'var(--font-mono)',
          fontSize: 'clamp(8px, 0.625em, 11px)',
          fontWeight: 600,
          color,
          letterSpacing: '0.08em',
        }}
      >
        {name}
      </div>
      <pre
        style={{
          margin: 0,
          padding: 'clamp(8px, 0.75em, 14px) clamp(10px, 1em, 16px)',
          fontFamily: 'var(--font-mono)',
          fontSize: 'clamp(9px, 0.7em, 13px)',
          lineHeight: 1.5,
          color: 'var(--color-text-primary)',
          whiteSpace: 'pre-wrap',
          wordBreak: 'break-all',
        }}
      >
        {code}
      </pre>
    </div>
  );
}

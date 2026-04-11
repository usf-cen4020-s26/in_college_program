import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';

/**
 * Slide 3 — How We Work: Epics → Stories → Subtasks hierarchy.
 * 3 build steps progressively reveal each tier.
 */
export function Slide03HowWeWork({ step }: SlideProps) {
  return (
    <SlideFrame act="PART A · AGILE AT A GLANCE" kicker="METHODOLOGY">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(40px, 5.5em, 88px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
        }}
      >
        How We <span className="text-gradient">Work.</span>
      </h1>

      <div
        style={{
          display: 'flex',
          flexDirection: 'column',
          gap: 'clamp(16px, 1.5em, 28px)',
          flex: 1,
          justifyContent: 'center',
        }}
      >
        {/* Epic tier */}
        <StepReveal currentStep={step} visibleAt={0}>
          <HierarchyCard
            level={1}
            label="Epic"
            description="A major functionality area"
            example="Job Posting, Messaging, Authentication"
            color="#0A66C2"
          />
        </StepReveal>

        {/* Story tier */}
        <StepReveal currentStep={step} visibleAt={1}>
          <div style={{ paddingLeft: 'clamp(24px, 3em, 48px)' }}>
            <HierarchyCard
              level={2}
              label="Story"
              description="A user-visible feature within an epic"
              example="As a user, I can post a job with title, description, employer, location, and salary"
              color="#38BDF8"
            />
          </div>
        </StepReveal>

        {/* Subtask tier */}
        <StepReveal currentStep={step} visibleAt={2}>
          <div style={{ paddingLeft: 'clamp(48px, 6em, 96px)' }}>
            <HierarchyCard
              level={3}
              label="Subtask"
              description="An implementation or testing unit"
              example="Implement salary validation, Write test for blank description re-prompt"
              color="#85C5FF"
            />
          </div>
        </StepReveal>
      </div>
    </SlideFrame>
  );
}

function HierarchyCard({
  level,
  label,
  description,
  example,
  color,
}: {
  level: number;
  label: string;
  description: string;
  example: string;
  color: string;
}) {
  return (
    <div
      style={{
        display: 'flex',
        alignItems: 'center',
        gap: 'clamp(12px, 1.25em, 24px)',
        padding: 'clamp(16px, 1.5em, 28px)',
        borderRadius: 16,
        background: 'var(--color-bg-card)',
        border: `1px solid ${color}33`,
        borderLeft: `4px solid ${color}`,
      }}
    >
      <div
        style={{
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          width: 'clamp(32px, 2.5em, 44px)',
          height: 'clamp(32px, 2.5em, 44px)',
          borderRadius: '50%',
          background: `${color}22`,
          color,
          fontFamily: 'var(--font-mono)',
          fontSize: 'clamp(14px, 1.125em, 20px)',
          fontWeight: 700,
          flexShrink: 0,
        }}
      >
        {level}
      </div>
      <div style={{ flex: 1, minWidth: 0 }}>
        <div
          style={{
            fontFamily: 'var(--font-display)',
            fontSize: 'clamp(18px, 1.75em, 32px)',
            fontWeight: 700,
            color,
            marginBottom: 4,
          }}
        >
          {label}
        </div>
        <div
          style={{
            fontFamily: 'var(--font-body)',
            fontSize: 'clamp(12px, 1em, 18px)',
            color: 'var(--color-text-muted)',
            lineHeight: 1.4,
          }}
        >
          {description}
        </div>
        <div
          style={{
            fontFamily: 'var(--font-mono)',
            fontSize: 'clamp(10px, 0.75em, 13px)',
            color: 'var(--color-text-dim)',
            marginTop: 6,
            fontStyle: 'italic',
          }}
        >
          e.g. {example}
        </div>
      </div>
    </div>
  );
}

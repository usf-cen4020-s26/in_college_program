import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';

/**
 * Slide 5 — Branching Strategy diagram.
 * Step 0: branches appear. Step 1: merge arrows animate.
 */
export function Slide05Branching({ step }: SlideProps) {
  return (
    <SlideFrame act="PART A · AGILE AT A GLANCE" kicker="GIT WORKFLOW">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(36px, 4.5em, 72px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          color: 'var(--color-text-primary)',
        }}
      >
        Branching <span className="text-gradient">Strategy.</span>
      </h1>

      <div
        style={{
          flex: 1,
          display: 'flex',
          flexDirection: 'column',
          gap: 'clamp(20px, 2em, 40px)',
          justifyContent: 'center',
          alignItems: 'center',
        }}
      >
        {/* Branch boxes */}
        <div
          style={{
            display: 'flex',
            gap: 'clamp(16px, 2.5em, 48px)',
            alignItems: 'center',
            flexWrap: 'wrap',
            justifyContent: 'center',
          }}
        >
          <BranchBox name="test/<feature>" color="#22C55E" label="Tests written here" />
          <BranchBox name="impl/<feature>" color="#38BDF8" label="Code written here" />
          <BranchBox name="main" color="#F59E0B" label="Stable, always passing" />
        </div>

        {/* Merge flow */}
        <StepReveal currentStep={step} visibleAt={1}>
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              gap: 'clamp(12px, 1.5em, 24px)',
              flexWrap: 'wrap',
              justifyContent: 'center',
            }}
          >
            <MergeStep from="test/*" to="impl/*" />
            <MergeStep from="impl/*" to="main" viaPR />
          </div>

          <p
            style={{
              margin: 0,
              marginTop: 'clamp(12px, 1.5em, 24px)',
              fontFamily: 'var(--font-body)',
              fontSize: 'clamp(12px, 1em, 18px)',
              color: 'var(--color-text-muted)',
              textAlign: 'center',
            }}
          >
            CI runs on every push. Tests must pass before merge.
          </p>
        </StepReveal>
      </div>
    </SlideFrame>
  );
}

function BranchBox({
  name,
  color,
  label,
}: {
  name: string;
  color: string;
  label: string;
}) {
  return (
    <div
      style={{
        padding: 'clamp(16px, 1.5em, 28px) clamp(20px, 2em, 36px)',
        borderRadius: 16,
        background: 'var(--color-bg-card)',
        border: `1px solid ${color}33`,
        borderTop: `3px solid ${color}`,
        textAlign: 'center',
        minWidth: 'clamp(160px, 14em, 260px)',
      }}
    >
      <div
        style={{
          fontFamily: 'var(--font-mono)',
          fontSize: 'clamp(18px, 1.5em, 26px)',
          fontWeight: 600,
          color,
          marginBottom: 8,
        }}
      >
        {name}
      </div>
      <div
        style={{
          fontFamily: 'var(--font-body)',
          fontSize: 'clamp(12px, 1em, 16px)',
          color: 'var(--color-text-dim)',
        }}
      >
        {label}
      </div>
    </div>
  );
}

function MergeStep({
  from,
  to,
  viaPR,
}: {
  from: string;
  to: string;
  viaPR?: boolean;
}) {
  return (
    <div
      style={{
        display: 'flex',
        alignItems: 'center',
        gap: 'clamp(8px, 0.75em, 14px)',
      }}
    >
      <span
        style={{
          fontFamily: 'var(--font-mono)',
          fontSize: 'clamp(12px, 1em, 16px)',
          color: 'var(--color-text-muted)',
        }}
      >
        {from}
      </span>
      <span
        style={{
          fontFamily: 'var(--font-mono)',
          fontSize: 'clamp(16px, 1.5em, 24px)',
          color: 'var(--color-brand-accent)',
        }}
      >
        →
      </span>
      <span
        style={{
          fontFamily: 'var(--font-mono)',
          fontSize: 'clamp(12px, 1em, 16px)',
          color: 'var(--color-text-muted)',
        }}
      >
        {to}
      </span>
      {viaPR && (
        <span
          style={{
            padding: '2px 10px',
            borderRadius: 999,
            background: 'rgba(245,158,11,0.15)',
            color: '#F59E0B',
            fontFamily: 'var(--font-body)',
            fontSize: 'clamp(9px, 0.7em, 12px)',
            fontWeight: 600,
            letterSpacing: '0.1em',
            textTransform: 'uppercase',
          }}
        >
          via PR
        </span>
      )}
    </div>
  );
}

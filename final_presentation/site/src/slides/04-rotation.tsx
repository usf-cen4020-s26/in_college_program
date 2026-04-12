import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';
import { ROLE_ROTATION } from '../data/roles-rotation';

const ROLE_COLORS: Record<string, string> = {
  scrumMaster: '#F59E0B',
  coder1: '#38BDF8',
  coder2: '#85C5FF',
  tester1: '#22C55E',
  tester2: '#5EEAD4',
};

/**
 * Slide 4 — Rotating Scrum Master table. Step 0 shows epics 1-5,
 * step 1 reveals epics 6-9.
 */
export function Slide04Rotation({ step }: SlideProps) {
  const firstHalf = ROLE_ROTATION.slice(0, 5);
  const secondHalf = ROLE_ROTATION.slice(5);

  return (
    <SlideFrame act="PART A · AGILE AT A GLANCE" kicker="ROLE ROTATION">
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
        Rotating <span className="text-gradient">Roles.</span>
      </h1>

      <div style={{ flex: 1, display: 'flex', flexDirection: 'column', gap: 4, minHeight: 0 }}>
        {/* Header */}
        <div style={{ display: 'grid', gridTemplateColumns: '0.5fr 1fr 1fr 1fr 1fr 1fr', gap: 4 }}>
          {['Epic', 'Scrum Master', 'Coder 1', 'Coder 2', 'Tester 1', 'Tester 2'].map((h) => (
            <div
              key={h}
              style={{
                padding: 'clamp(6px, 0.5em, 10px) clamp(8px, 0.75em, 14px)',
                fontFamily: 'var(--font-body)',
                fontSize: 'clamp(9px, 0.7em, 12px)',
                fontWeight: 600,
                letterSpacing: '0.15em',
                textTransform: 'uppercase',
                color: 'var(--color-text-dim)',
              }}
            >
              {h}
            </div>
          ))}
        </div>

        {/* First half — always visible */}
        <StepReveal currentStep={step} visibleAt={0}>
          {firstHalf.map((row) => (
            <RotationRow key={row.epic} row={row} />
          ))}
        </StepReveal>

        {/* Second half — step 1 */}
        <StepReveal currentStep={step} visibleAt={1}>
          {secondHalf.map((row) => (
            <RotationRow key={row.epic} row={row} />
          ))}
        </StepReveal>
      </div>
    </SlideFrame>
  );
}

function RotationRow({ row }: { row: typeof ROLE_ROTATION[number] }) {
  const cells = [
    { value: String(row.epic), color: 'var(--color-text-primary)' },
    { value: row.scrumMaster, color: ROLE_COLORS.scrumMaster },
    { value: row.coder1, color: ROLE_COLORS.coder1 },
    { value: row.coder2, color: ROLE_COLORS.coder2 },
    { value: row.tester1, color: ROLE_COLORS.tester1 },
    { value: row.tester2, color: ROLE_COLORS.tester2 },
  ];

  return (
    <div
      style={{
        display: 'grid',
        gridTemplateColumns: '0.5fr 1fr 1fr 1fr 1fr 1fr',
        gap: 4,
        marginBottom: 4,
      }}
    >
      {cells.map((cell, i) => (
        <div
          key={i}
          style={{
            padding: 'clamp(8px, 0.6em, 12px) clamp(8px, 0.75em, 14px)',
            borderRadius: 8,
            background: 'var(--color-bg-card)',
            fontFamily: 'var(--font-body)',
            fontSize: 'clamp(11px, 0.875em, 15px)',
            fontWeight: i === 0 ? 700 : 500,
            color: cell.color,
          }}
        >
          {cell.value}
        </div>
      ))}
    </div>
  );
}

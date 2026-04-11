import type { SlideProps } from './types';
import { SlideFrame } from '../components/SlideFrame';
import { StepReveal } from '../components/StepReveal';

interface DataFile {
  name: string;
  recordSize: string;
  maxRecords: string;
  purpose: string;
}

const GROUP_1: DataFile[] = [
  { name: 'ACCOUNTS.DAT', recordSize: '62 B', maxRecords: '5', purpose: 'User credentials' },
  { name: 'PROFILES.DAT', recordSize: '195 B', maxRecords: '5', purpose: 'User profile data' },
  { name: 'PENDING.DAT', recordSize: '42 B', maxRecords: '25', purpose: 'Pending connection requests' },
  { name: 'CONNECTIONS.DAT', recordSize: '42 B', maxRecords: '25', purpose: 'Accepted connections' },
];

const GROUP_2: DataFile[] = [
  { name: 'JOBS.DAT', recordSize: '326 B', maxRecords: '10', purpose: 'Job postings' },
  { name: 'APPLICATIONS.DAT', recordSize: '166 B', maxRecords: '50', purpose: 'Job applications' },
  { name: 'MESSAGES.DAT', recordSize: '265 B', maxRecords: '100', purpose: 'User messages' },
];

function FileRow({ file }: { file: DataFile }) {
  return (
    <div
      style={{
        display: 'grid',
        gridTemplateColumns: '1.4fr 0.7fr 0.7fr 1.8fr',
        gap: 16,
        padding: '16px 22px',
        borderRadius: 14,
        background: 'var(--color-bg-panel)',
        border: '1px solid rgba(112,181,249,0.12)',
        boxShadow: '0 8px 24px -12px rgba(0,0,0,0.5)',
        alignItems: 'center',
      }}
    >
      <div
        style={{
          fontFamily: 'var(--font-mono)',
          fontWeight: 700,
          fontSize: 'clamp(14px, 1.05em, 18px)',
          color: 'var(--color-text-primary)',
        }}
      >
        {file.name}
      </div>
      <div
        style={{
          fontFamily: 'var(--font-mono)',
          fontSize: 'clamp(12px, 0.9em, 15px)',
          color: 'var(--color-brand-accent)',
        }}
      >
        {file.recordSize}
      </div>
      <div
        style={{
          fontFamily: 'var(--font-mono)',
          fontSize: 'clamp(12px, 0.9em, 15px)',
          color: 'var(--color-text-muted)',
        }}
      >
        max {file.maxRecords}
      </div>
      <div
        style={{
          fontFamily: 'var(--font-body)',
          fontSize: 'clamp(13px, 0.95em, 16px)',
          color: 'var(--color-text-muted)',
        }}
      >
        {file.purpose}
      </div>
    </div>
  );
}

/**
 * Slide 55 — Data Architecture: Seven .DAT Files.
 */
export function Slide55DataArch({ step }: SlideProps) {
  return (
    <SlideFrame act="PART D · ARCHITECTURE" kicker="DATA LAYER">
      <h1
        style={{
          margin: 0,
          fontFamily: 'var(--font-display)',
          fontSize: 'clamp(48px, 5.5em, 88px)',
          fontWeight: 700,
          letterSpacing: '-0.02em',
          lineHeight: 1.05,
        }}
      >
        <span className="text-gradient">Seven .DAT Files</span>
      </h1>

      {/* Column headers */}
      <div
        style={{
          display: 'grid',
          gridTemplateColumns: '1.4fr 0.7fr 0.7fr 1.8fr',
          gap: 16,
          padding: '0 22px',
        }}
      >
        {['File', 'Record', 'Capacity', 'Purpose'].map((h) => (
          <div
            key={h}
            style={{
              fontFamily: 'var(--font-body)',
              fontSize: 12,
              fontWeight: 700,
              letterSpacing: '0.18em',
              textTransform: 'uppercase',
              color: 'var(--color-text-dim)',
            }}
          >
            {h}
          </div>
        ))}
      </div>

      <div
        style={{
          flex: 1,
          display: 'flex',
          flexDirection: 'column',
          gap: 10,
        }}
      >
        <StepReveal currentStep={step} visibleAt={0}>
          <div style={{ display: 'flex', flexDirection: 'column', gap: 10 }}>
            {GROUP_1.map((f) => (
              <FileRow key={f.name} file={f} />
            ))}
          </div>
        </StepReveal>

        <StepReveal currentStep={step} visibleAt={1}>
          <div style={{ display: 'flex', flexDirection: 'column', gap: 10 }}>
            {GROUP_2.map((f) => (
              <FileRow key={f.name} file={f} />
            ))}
          </div>
        </StepReveal>
      </div>
    </SlideFrame>
  );
}

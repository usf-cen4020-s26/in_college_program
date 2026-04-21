/**
 * Theme tokens consumed by every slide and Remotion composition.
 * Mirrored by CSS custom properties in globals.css so Tailwind utilities
 * and inline styles stay in lockstep.
 */

export const palette = {
  bg: {
    base: '#050814',
    panel: '#111d3a',
    elevated: '#1a2847',
    card: '#141e38',
    code: '#0B1222',
    codeBorder: '#1E293B',
  },
  brand: {
    primary: '#0A66C2',
    accent: '#85C5FF',
    sky: '#38BDF8',
    deep: '#082F5C',
  },
  text: {
    primary: '#F5F7FA',
    muted: '#B0BAD0',
    dim: '#7889A5',
  },
  state: {
    success: '#22C55E',
    warn: '#F59E0B',
    danger: '#EF4444',
  },
  grad: {
    headline: 'linear-gradient(135deg, #0A66C2 0%, #38BDF8 55%, #85C5FF 100%)',
    glow: 'radial-gradient(ellipse 1200px 600px at 50% -200px, rgba(10,102,194,0.45) 0%, rgba(10,102,194,0.0) 70%)',
  },
} as const;

export const typography = {
  display: '"IBM Plex Sans", "Inter", system-ui, sans-serif',
  body: '"Inter", system-ui, sans-serif',
  mono: '"JetBrains Mono", "IBM Plex Mono", ui-monospace, monospace',
  stat: '"IBM Plex Mono", ui-monospace, monospace',
} as const;

export const spacing = {
  page: 96,
  gap: 48,
  radius: 20,
  radiusSm: 12,
} as const;

export const motion = {
  fps: 30,
  slideDurationFrames: 180,
  terminalFps: 30,
  easeOut: [0.16, 1, 0.3, 1] as const,
} as const;

export const deck = {
  width: 1920,
  height: 1080,
} as const;

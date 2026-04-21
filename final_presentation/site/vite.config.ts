import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import tailwindcss from '@tailwindcss/vite';

// The repo hosts multiple things at its root; the deck lives under
// final_presentation/site/ and is published to GitHub Pages at
// https://usf-cen4020-s26.github.io/in_college_program/.
// Vite needs this base path so asset URLs resolve under the Pages subpath.
const base = process.env.DECK_BASE ?? '/in_college_program/';

export default defineConfig({
  base,
  plugins: [react(), tailwindcss()],
  server: {
    port: 5173,
    strictPort: false,
  },
  build: {
    target: 'es2022',
    sourcemap: false,
    chunkSizeWarningLimit: 1500,
  },
});

# InCollege · Final Presentation

A React + Remotion slide deck used for the 25-minute CEN 4020 final VC pitch.
Hosted on GitHub Pages; everything that powers it lives under this
`final_presentation/` directory.

## Structure

```
final_presentation/
├── PLAN.md                               ← the approved plan (source of truth)
├── Final Presentation Guidelines.pdf     ← the rubric the plan maps to
├── README.md                             ← you are here
├── scripts/
│   └── verify.sh                         ← diffs slide code snippets vs real COBOL
└── site/                                 ← the Vite + React + Remotion app
    ├── package.json
    ├── vite.config.ts
    ├── public/assets/                    ← burndown PNGs, Jira.png, Github.png, Roles.txt
    └── src/
        ├── Deck.tsx                      ← shell (keyboard nav, URL sync, fullscreen)
        ├── theme.ts                      ← color + typography tokens
        ├── globals.css                   ← Tailwind v4 @theme block + deck CSS
        ├── shiki.ts                      ← singleton COBOL highlighter
        ├── slides/                       ← 25 slide components
        ├── components/                   ← shared UI (SlideFrame, PersonCard, …)
        ├── remotion/compositions/        ← Terminal, StatCounter, CodeReveal, …
        └── data/                         ← team, stats, security, storage, snippets, terminal scripts
```

## Daily loop

```sh
# from final_presentation/site/
bun install                # once
bun run dev                # http://localhost:5173 — hot reload, arrow keys to navigate
bun run build              # production build to dist/
bun run preview            # serve the built site offline (wifi fallback for classroom)
```

### Deck keyboard shortcuts

| key | action |
|---|---|
| `←` / `PageUp` / `p` | previous slide |
| `→` / `PageDown` / `Space` / `n` | next slide |
| `Home` / `End` | jump to first / last |
| `1`–`9` | jump to slide 1–9 |
| `F` | toggle fullscreen |
| `H` | hide/show the controls chrome |

### Deep linking

Any slide is directly linkable via `?slide=N` (1-based). Example:
`https://usf-cen4020-s26.github.io/in_college_program/?slide=12` opens the
messaging arc demo.

## Verifying slide snippets against the real COBOL

Three slides (16, 17, 18) embed verbatim excerpts from `src/*.cpy`. Run
`./scripts/verify.sh` from the repo root to diff them against the live
sources — any drift fails the script.

## Deploying

The `.github/workflows/deploy-pages.yml` workflow rebuilds and publishes to
GitHub Pages on any push to `main` or `feat/final_presentation` that touches
`final_presentation/site/**`. You can also trigger it manually from the
Actions tab ("Deploy final presentation to GitHub Pages" → Run workflow).

The public URL after deployment is:
`https://usf-cen4020-s26.github.io/in_college_program/`

## Canvas submission

Paste the live URL into the Canvas submission box. The prof has confirmed a
link is acceptable in lieu of a `.pptx` file. If classroom wifi ever dies
mid-talk, plug a team laptop into the projector and run `bun run preview`
against `site/dist/` — same build, served from `localhost:4173`.

## Who presents what

See `PLAN.md` §5 for the speaker-by-slide assignment. Every team member has
at least two slide blocks. Scrum master opens and closes.

## Design

See `PLAN.md` §6. All tokens come from `site/src/theme.ts` and
`site/src/globals.css`. Do not hardcode colors in individual slides.

## Team

| Name | Role |
|---|---|
| Trevor Flahardy | Scrum Master |
| Aaron Fraze | Coder #1 |
| Olga Druzhkova | Coder #2 |
| Melaine Fernandez Sarduy | Tester #1 |
| Victoria Field | Tester #2 |

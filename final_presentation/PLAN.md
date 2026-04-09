# Final Presentation Plan — InCollege COBOL VC Pitch (Website Edition)

---

## 1. Context

**The ask.** The class handed us two documents that set the constraints:

1. **`final_presentation/Final Presentation Guidelines.pdf`** — reframes the talk as a **25-minute VC pitch** (strictly enforced). VCs want to see the product work, hear how we built it, and see a team that works together. The prof has explicitly confirmed we do **not** need to submit a PowerPoint — a **link to a hosted website is acceptable**.
2. **`Lectures/CEN 4020 - Lesson 24.pdf`** — the original Epic #9 rubric that asks for stories, owners, burndowns, and a tour of prior epics. Still in force as background color; the new guidelines are the primary rubric.

**The rubric has drifted since our codebase was scoped.** The guidelines list 15 demo features but 5 of them aren't in our program (pre-login search, English/Spanish, saved jobs, plus/standard tiers, system-wide notifications). Those bullets are outdated content and we don't demo them, apologize for them, or roadmap them. We present what we've built.

**Team** (`README.md:477-485`, `.dev/local/epic_9_submission/Roles.txt`):

| # | Name | Role |
|---|---|---|
| 1 | **Trevor Flahardy** | Scrum Master |
| 2 | **Aaron Fraze** (`fraze-dev`) | Coder #1 — implemented `7840-VIEW-MESSAGES` |
| 3 | **Olga Druzhkova** (Оля Дружкова) | Coder #2 — display formatting (MSW-466/467), docs audit |
| 4 | **Melaine Fernandez Sarduy** | Tester #1 — seed harness, fixture packaging |
| 5 | **Victoria Field** | Tester #2 — positive/negative/edge/persistence fixtures |

**Why this plan exists.** We want a presentation artifact that is genuinely stunning — modern, animated, cohesive — that both lands as a VC pitch *and* shows off our engineering taste. A hosted website built with React + Remotion clears the ceiling a rasterized PowerPoint cannot. Everything lives in `final_presentation/`; the rest of the repo is untouched.

---

## 2. Deliverable Strategy — A Hosted React + Remotion Website

The artifact is a **single-page React app** with a custom 25-slide deck experience, embedded Remotion compositions for every animated moment, and Tailwind CSS v4 for the layout shell. It's deployed to **GitHub Pages** via a GitHub Actions workflow. The URL is what gets submitted to Canvas.

```
Vite + React + TypeScript + Tailwind v4
   │
   ├── Deck shell (arrow-key nav, slide counter, URL sync, fullscreen)
   │        │
   │        └── renders one <Slide> component per visible slide
   │
   ├── <Slide> mounts a Remotion <Player> for animated content
   │        │
   │        └── Player plays a composition at 1920×1080, 30fps
   │
   └── Shiki (browser bundle) for COBOL syntax highlighting

   vite build → dist/   →   GitHub Actions → gh-pages branch
                                                 │
                                                 ▼
                 https://<org>.github.io/<repo>/  ← Canvas link
```

**Why a website beats a rasterized pptx for this specific pitch:**

- **Real motion survives.** Entry animations, typewriter terminal playbacks, code reveal-line-by-line, counter-up stats, chart grow-in — they all play live instead of being flattened to a PNG.
- **Unified codebase.** One repo, one design system, one build. No dual-output pipeline, no PNG → pptx rasterization step, no drift.
- **Shareable after the talk.** The VCs in the room and anyone else can visit the URL later. Much better leave-behind than a zipped pptx.
- **Update in seconds.** Fix a typo → `git push` → GitHub Actions redeploys in under a minute. No re-rendering stills, no rebuilding pptx.
- **Deep linking.** `?slide=14` jumps straight to the fixture matrix. Great for rehearsal, Q&A backtracking, and fixing issues on the fly.
- **Prof explicitly approved this route.** The only hard constraint from Lesson 24 ("download and have them waiting") was dropped when he said a link is fine. Nothing to work around.

**Fallback plan** (genuinely free, no extra engineering): the whole site runs locally via `bun run preview` against the built `dist/` folder. If classroom wifi dies, we plug a team laptop into the projector and serve the same build offline from `localhost`. No separate artifact required.

---

## 3. Assets We Already Have (Reuse, Don't Recreate)

`.dev/local/epic_9_submission/` already holds the Canvas-submission artifacts. They become direct slide content:

| File | Used On Slide | Purpose |
|---|---|---|
| `burndown_start.png` | Slide: Burndown (Monday) | Apr 3 — ~26 story points, "Today" marker at sprint start |
| `burndown_end.png` | Slide: Burndown (Sprint End) | Apr 9 — staircase descent, zero at deadline |
| `Jira.png` | Slide: Jira Board / Stories | Full Jira board — stories, tasks, owners |
| `Github.png` | Slide: Sprint Timeline | Chronological GitHub commit list |
| `Roles.txt` | Slide: Team | Authoritative role list |
| `InCollege-Input.txt` | Feature demo fallback | Sample input for live replay during demos |
| `InCollege-Output.txt` | Feature demo fallback | Matching expected output |

These files get copied into `final_presentation/site/public/assets/` so React components resolve them via `/assets/burndown_end.png`.

---

## 4. The 25-Minute VC Pitch Narrative

25 minutes (strictly enforced) across **25 slides** ≈ **60 seconds per slide**. Five speakers × roughly 5 minutes each. Every role gets the mic twice.

### Act I — The Pitch (Trevor) — ~3 min

| # | Slide | Purpose | Visual |
|---|---|---|---|
| 01 | **Title** | "InCollege — A career-networking platform, in COBOL." Team name, date. | Dark gradient, animated InCollege wordmark, team city codename drifting in |
| 02 | **Meet the Team** | Five people, five roles, ~10 seconds each | 5 person cards with avatar monograms, role chip, staggered entry animation |
| 03 | **The Product** | 30-second pitch: what it is, who it's for, why it matters | Big serif headline ("Where college meets career"), subtle particle background, three supporting phrases that fade in |
| 04 | **By the Numbers** | Counter-up stats on entry | **9** Epics · **25** modules · **2,200+** LOC · **195** tests · **7** persistence files. Each number animates from 0 on slide enter. |

### Act II — Product Demo (Aaron, Olga, Victoria, Melaine) — ~11 min

This is the core of the pitch. Each slide is one or two features shown as an **animated terminal playback** — a styled mock console that types inputs character-by-character and prints the program's responses with realistic timing. It feels like watching a real session at 2× speed. The playbacks are driven by Remotion sequences reading from recorded `.in.txt` / `.out.txt` pairs already committed to the tests/fixtures tree.

| # | Slide | Speaker | Feature(s) Shown | How It's Built |
|---|---|---|---|---|
| 05 | **Create Your Account** | Aaron | Password policy enforcement, account cap, persistence | Terminal playback of `tests/fixtures/login/*.in.txt` for a happy path; right-side card with the 4 password rules from `AUTH.cpy:218-280` |
| 06 | **Build Your Profile** | Aaron | Required + optional fields, experience, education | Terminal playback creating a profile with the full form, then jumping to "View My Profile" |
| 07 | **Find Someone You Know** | Olga | Full-name search, exact match, found-vs-not-found | Terminal playback searching for a user, then opening their profile card on the right of the slide |
| 08 | **Connect → Accept → Network** | Olga | Send request, accept on another account, list network — three-step arc | Split-screen terminal: Alice's session on the left sends a request, Bob's session on the right accepts, Alice sees Bob in `View My Network` |
| 09 | **Post a Job** | Victoria | Required-field re-prompting, optional salary | Terminal playback of `tests/fixtures/job_internship_posting/*.in.txt`; on-slide callout showing the re-prompt loop kicking in when the title is blank |
| 10 | **Browse & Apply** | Victoria | List view → detail view → apply → confirmation | Terminal playback walking through the Job Search submenu, picking a posting, applying, seeing the confirmation line |
| 11 | **My Applications** | Victoria | Per-user application report | Terminal playback of `View My Applications`; on-slide comparison showing the report filters by logged-in user |
| 12 | **Messaging: The Full Arc** | Melaine | Connection-gated send + chronological view + recipient isolation | Split-screen: left session (Alice) sends a message to Bob; right session (Bob) logs in, opens `View My Messages`, sees it. Bottom band calls out: "Alice's inbox stays empty — recipient isolation." This slide is Epic #9's *demo*. |

### Act III — How We Built It (Trevor + Olga) — ~8 min

| # | Slide | Speaker | Purpose | Visual |
|---|---|---|---|---|
| 13 | **Architecture: 25 Modules** | Trevor | Answer "What modules / functions / classes?" | Animated graph: `main.cob` in the center, 24 copybooks orbiting in 5 colored clusters (Core, Auth, Profiles/Discovery, Jobs, Messaging, Data Models). Hovering / entry-animation highlights each cluster in turn. |
| 14 | **Copybook Deep Dive** | Trevor | Show what "modular" means in COBOL terms | Three-column list of the 24 copybooks grouped by concern (Feature / Working Storage / I/O). One-line description each, Shiki-highlighted. |
| 15 | **Data Storage** | Olga | Answer "How is data stored and retrieved?" | Seven file cards (ACCOUNTS, PROFILES, PENDING, CONNECTIONS, JOBS, APPLICATIONS, MESSAGES) each with record layout, capacity, and rewrite-vs-append strategy. Cards flip in on entry. |
| 16 | **Code Gem #1 — Recursive Message Read** | Olga | Show engineering taste | Shiki-highlighted snippet of `src/VIEWMESSAGE.cpy:75-120` (`7841-VIEW-MESSAGES-LOOP`) with lines revealing top-to-bottom. Callouts on `AT END`, the recipient filter, and the tail recurse. |
| 17 | **Code Gem #2 — Bidirectional Connection Check** | Olga | Graph-theory matters even in COBOL | Snippet of `src/SENDMESSAGE.cpy:205-224` with two arrows highlighting the (A→B) and (B→A) branches. Beneath: a tiny animated graph showing the undirected edge being validated. |
| 18 | **Security Features** | Trevor | Answer "What security features?" | Six cards with file:line evidence: password policy (`AUTH.cpy:218-280`), account cap, masked echo, recipient isolation, connection-gated messaging, duplicate guards on applications. |
| 19 | **Testing at Scale** | Trevor | Answer "How many test cases?" | Big **195** counter-up, then a 3×3 grid of test categories with their counts. Beneath: "**Zero mocks. Real COBOL binary. Deterministic diffs.**" |
| 20 | **How the Harness Works** | Trevor | How we *know* everything works | Flow diagram: `*.in.txt` → preprocessor (seed macros) → `INPUT.TXT` → `bin/main` → `OUTPUT.TXT` → macro expansion + timestamp normalization → `difflib` → ✅/❌. Arrows animate left-to-right. |

### Act IV — Team & Process (Trevor) — ~2 min

| # | Slide | Purpose | Visual |
|---|---|---|---|
| 21 | **How We Ran the Sprint** | Branching, PRs #35 + #36, daily cadence | Vertical timeline Apr 1 → Apr 9 with commit density heatmap on the left and `Github.png` as a subtle background on the right |
| 22 | **Burndown — Start** | Required rubric deliverable | Full-bleed `burndown_start.png` with a soft glow; one-line annotation at the top |
| 23 | **Burndown — End** | Required rubric deliverable | Full-bleed `burndown_end.png` with callouts on the big drop Apr 6→7 (impl merge), Apr 7→8 (test merge), and the zero on Apr 9 |

### Act V — Close (all 5 speakers) — ~1 min

| # | Slide | Speaker | Purpose | Visual |
|---|---|---|---|---|
| 24 | **What's Next** | Aaron + Olga + Victoria + Melaine (one line each) | Each teammate names one thing they learned or one thing they're proudest of | Five cards, one teammate's quote each, illuminated one at a time |
| 25 | **Thank You / Q&A** | Trevor | Wrap-up | Animated team signature, repo link, QR code to the live site, "Questions?" centerpiece |

---

## 5. Speaker Assignments

| Slides | Speaker | Runtime |
|---|---|---|
| 01–04 | **Trevor** (Scrum Master) — opens, introduces team, sets the pitch | ~3 min |
| 05–06 | **Aaron** (Coder #1) — account + profile | ~2 min |
| 07–08 | **Olga** (Coder #2) — search + connections arc | ~2 min |
| 09–11 | **Victoria** (Tester #2) — jobs arc | ~3 min |
| 12 | **Melaine** (Tester #1) — messaging (Epic #9's demo moment) | ~1.5 min |
| 13–14 | **Trevor** — architecture + copybooks | ~2 min |
| 15 | **Olga** — data storage | ~1 min |
| 16–17 | **Olga** — code gems | ~1.5 min |
| 18–20 | **Trevor** — security + testing + harness | ~3 min |
| 21–23 | **Trevor** — sprint + burndown | ~2 min |
| 24 | **Aaron + Olga + Victoria + Melaine** (one card each) | ~1 min |
| 25 | **Trevor** — close + Q&A | ~0.5 min |

Everyone speaks at least twice. Every role is represented. Scrum master opens and closes; coders and testers split the middle. Total budgeted ~22.5 min with ~2.5 min of slack.

---

## 6. Visual Design System

Encoded once in `final_presentation/site/src/theme.ts` + a single `globals.css` with Tailwind v4 theme tokens. No ad-hoc styling inside individual slides.

**Palette (dark, brand-aligned):**

- `bg.base` `#050814` (near-black with a hint of blue)
- `bg.panel` `#0C1327` · `bg.elevated` `#131C37`
- `brand.primary` `#0A66C2` (InCollege blue) · `brand.accent` `#70B5F9`
- `brand.glow` radial gradient centered top, 40% brand.primary → transparent
- `text.primary` `#F5F7FA` · `text.muted` `#9CA3B4` · `text.dim` `#5F6B82`
- `success` `#22C55E` · `warn` `#F59E0B` · `danger` `#EF4444`
- `code.bg` `#0B1222` with 1 px `#1E293B` border and subtle inner glow
- Accent gradients: `from-[#0A66C2] via-[#38BDF8] to-[#70B5F9]` used sparingly on headlines and the wordmark

**Typography (via `@remotion/google-fonts`, no local file management):**

- Headings — **IBM Plex Sans**, 700, −0.02em tracking, fluid sizes clamped to viewport
- Body — **Inter**, 500
- Code — **JetBrains Mono**, 500, ligatures off
- Stats — **IBM Plex Mono**, 700, tabular figures

**Layout language:**

- 16:9 deck container, fluid to viewport, max 1920×1080
- 96 px outer padding, 48 px grid gap, 12-column grid
- Every slide has: top-left `NN / 25` slide index, top-right small InCollege wordmark, bottom rule with section name ("ACT II · PRODUCT DEMO")
- All slides sit on an absolutely positioned glow layer so the brand color bleeds through corners
- Code snippets live inside a `<CodePanel>` component with a terminal-style title bar (fake traffic lights + file path + line range, e.g., `src/VIEWMESSAGE.cpy:75-120`)

**Motion policy — lean into it, we finally can:**

- **Slide entry**: 14-frame ease-out fade + 12 px Y-translate for every child; sliding between slides uses `TransitionSeries` with `fade` + subtle slide-push
- **Terminal playbacks** (the Act II centerpiece): frame-by-frame typewriter for the user input, then blocked-out response printing for the COBOL program output. One Remotion composition per demo, reading input/expected pairs from fixture files at build time. The terminal cursor blinks at a deterministic rate. Runtime ~20–30 s per demo, auto-plays on slide enter, looping forever.
- **Code reveals** (slides 14, 16, 17, 18): lines cascade in top-to-bottom, each with an 80 ms stagger, giving the audience a beat to read each line.
- **Counter-up stats**: `useCurrentFrame` + `interpolate` drive numbers from 0 to target over ~25 frames.
- **Architecture graph** (slide 13): 24 copybook nodes spring out from `main.cob` using `@remotion/shapes` and `interpolate` with a stagger — feels like constellation mapping.
- **Cross-slide transitions**: 300 ms fade-through + 16 px push, all CPU-cheap so a projector doesn't choke.

Every animation is deterministic (`useCurrentFrame`) so it plays identically every time — zero "demo effect" risk.

---

## 7. Project Layout

```
final_presentation/
├── PLAN.md                      ← this document
├── README.md                    ← one-page runbook + how to preview locally
├── Final Presentation Guidelines.pdf   ← the new rubric (committed for reference)
├── site/
│   ├── package.json             ← vite, react, react-dom, @remotion/player, remotion,
│   │                              @remotion/google-fonts, @remotion/shapes,
│   │                              @remotion/transitions, tailwindcss@next, shiki, zod
│   ├── vite.config.ts           ← React + Tailwind v4 plugin, base path for Pages
│   ├── tsconfig.json
│   ├── index.html               ← root div + font preload + meta tags
│   ├── public/
│   │   └── assets/              ← burndown_*.png, Jira.png, Github.png, Roles.txt,
│   │                              sample I/O text files, favicon
│   └── src/
│       ├── main.tsx             ← React entry, mounts <Deck />
│       ├── Deck.tsx             ← arrow-key nav, URL sync (?slide=N), fullscreen,
│       │                          slide counter overlay, keyboard shortcuts (←/→/F/0-9)
│       ├── slides/
│       │   ├── index.ts         ← exported slide list (id + component + speaker + section)
│       │   ├── 01-title.tsx
│       │   ├── 02-team.tsx
│       │   ├── 03-product.tsx
│       │   ├── 04-by-the-numbers.tsx
│       │   ├── 05-create-account.tsx
│       │   ├── 06-build-profile.tsx
│       │   ├── 07-find-someone.tsx
│       │   ├── 08-connect-accept-network.tsx
│       │   ├── 09-post-job.tsx
│       │   ├── 10-browse-apply.tsx
│       │   ├── 11-my-applications.tsx
│       │   ├── 12-messaging-arc.tsx
│       │   ├── 13-architecture.tsx
│       │   ├── 14-copybook-deep-dive.tsx
│       │   ├── 15-data-storage.tsx
│       │   ├── 16-code-gem-view-loop.tsx
│       │   ├── 17-code-gem-bidirectional.tsx
│       │   ├── 18-security.tsx
│       │   ├── 19-testing-at-scale.tsx
│       │   ├── 20-harness-flow.tsx
│       │   ├── 21-sprint-timeline.tsx
│       │   ├── 22-burndown-start.tsx
│       │   ├── 23-burndown-end.tsx
│       │   ├── 24-whats-next.tsx
│       │   └── 25-close.tsx
│       ├── remotion/
│       │   ├── compositions/
│       │   │   ├── Terminal.tsx     ← generic animated terminal (shared by slides 05-12)
│       │   │   ├── StatCounter.tsx  ← big-number count-up used on slide 04 + 19
│       │   │   ├── CodeReveal.tsx   ← line-by-line Shiki reveal used on slides 14, 16, 17
│       │   │   ├── ArchGraph.tsx    ← constellation layout for slide 13
│       │   │   └── HarnessFlow.tsx  ← left-to-right arrow flow for slide 20
│       │   └── index.tsx            ← composition registry for the Player
│       ├── components/
│       │   ├── SlideFrame.tsx       ← page number, wordmark, act label, background glow
│       │   ├── PersonCard.tsx
│       │   ├── StatCard.tsx
│       │   ├── FeatureCallout.tsx
│       │   ├── CodePanel.tsx        ← terminal-style chrome around Shiki output
│       │   └── Wordmark.tsx
│       ├── data/
│       │   ├── team.ts
│       │   ├── stats.ts             ← counts the slides reference (modules, tests, LOC)
│       │   ├── security.ts          ← the 6 security features with file:line citations
│       │   ├── storage.ts           ← the 7 persistence files with layouts
│       │   ├── snippets/            ← exact file text for the code-gem slides
│       │   │   ├── view-loop.cob           ← src/VIEWMESSAGE.cpy:75-120
│       │   │   ├── bidirectional.cob       ← src/SENDMESSAGE.cpy:205-224
│       │   │   └── password-policy.cob     ← src/AUTH.cpy:218-280
│       │   └── terminals/           ← terminal playback scripts (input + expected output)
│       │       ├── create-account.ts
│       │       ├── build-profile.ts
│       │       ├── find-someone.ts
│       │       ├── connect-accept.ts
│       │       ├── post-job.ts
│       │       ├── browse-apply.ts
│       │       ├── my-applications.ts
│       │       └── messaging-arc.ts
│       ├── shiki.ts               ← singleton highlighter, `cobol` + custom theme
│       ├── theme.ts               ← palette, typography, spacing tokens
│       └── globals.css            ← Tailwind v4 @theme block, font loading hooks
├── .github/
│   └── workflows/
│       └── deploy-pages.yml      ← build + publish to gh-pages on push to main
└── scripts/
    └── verify.sh                 ← diffs snippets against real files, checks asset paths
```

**Key architectural decisions (with citations):**

1. **Custom React deck shell, not a generic slide framework.** We want full control over transitions, URL deep links, and slide sequencing. Remotion supplies the animation engine; the deck shell is ~150 lines of React.
2. **`@remotion/player` mounted inside each slide.** Player is the official first-party way to embed a Remotion composition in a React app and runs every animation in the browser (`remotion.dev/docs/player`). Each slide either uses the Player to show an animated composition or renders pure React/Tailwind for the static bits.
3. **One `<Terminal>` composition, parameterized per demo.** The animated terminal — the heart of Act II — lives in one place and consumes a script array (`{t: 200, type: "1\n"}`, `{t: 1200, print: "Welcome, Alice!"}`). Each demo slide imports a script from `data/terminals/`. Scripts are authored by diffing real fixture files in `tests/fixtures/` against their expected outputs, so the playbacks match reality.
4. **Shiki in the browser for COBOL highlighting.** Research confirmed `cobol.json` is bundled in `shikijs/textmate-grammars-themes`, which backs Shiki. A singleton highlighter initializes once on first slide mount and reuses across all code-reveal slides. Renders run client-side — cheap, accurate, and styled to our palette via a custom Shiki theme.
5. **Data lives outside components.** Every string, stat, file path, code snippet, terminal script, and citation lives in `src/data/`. Slide components only do layout. The five teammates can edit text without touching motion code — cleaner merge conflicts.
6. **Snippets are exact file text.** Files under `data/snippets/` are verbatim from `src/*.cpy`. `scripts/verify.sh` diffs them against the live source at CI time — slides can't drift.
7. **GitHub Pages deploy via Actions.** Push to `main` → `deploy-pages.yml` runs `bun install && bun run build` → publishes `site/dist/` to the `gh-pages` branch → site is live at `https://<org>.github.io/<repo>/`. Using `base: '/epic_1/'` (or whatever the repo name is) in `vite.config.ts` so asset paths resolve under the Pages subpath.

---

## 8. Build / Preview / Deploy Workflow

```bash
# 0. one-time setup
cd final_presentation/site
bun install                          # or pnpm

# 1. dev loop — hot-reload every slide
bun run dev                          # → http://localhost:5173
# use ← / → arrow keys to navigate, F to toggle fullscreen
# use ?slide=N in the URL to jump to any slide

# 2. production build + local preview
bun run build                        # → site/dist/
bun run preview                      # → http://localhost:4173 (offline fallback)

# 3. deploy via push
git push origin main                 # Actions workflow publishes to gh-pages
# visit https://<org>.github.io/<repo>/ after ~60 s

# 4. verify that slide snippets still match the real COBOL files
../scripts/verify.sh                 # diff-zero or exit 1
```

**Verification checklist before the presentation:**

- [ ] All 25 slides render cleanly at 1920×1080 *and* at typical classroom resolutions (1280×720, 1366×768).
- [ ] Arrow-key navigation works, URL sync holds across refresh, fullscreen toggle works.
- [ ] All 8 terminal playbacks auto-play on slide enter and loop.
- [ ] Shiki highlighter initializes without errors in the Network tab; no CLS on first code-reveal slide.
- [ ] Slide 16 COBOL snippet equals the real file: `diff <(sed -n '75,120p' ../src/VIEWMESSAGE.cpy) site/src/data/snippets/view-loop.cob` → zero diff.
- [ ] Slide 17 snippet equivalent diff against `SENDMESSAGE.cpy:205-224`.
- [ ] Slide 18 snippet equivalent diff against `AUTH.cpy:218-280`.
- [ ] Slide 19 stats match reality — `./run_tests.sh` from repo root shows 195 passes and per-category counts match `data/stats.ts`.
- [ ] Slides 22 and 23 full-bleed images match `.dev/local/epic_9_submission/burndown_start.png` / `burndown_end.png`.
- [ ] `bun run preview` serves the deck offline end-to-end — tested on a team laptop with wifi disabled.
- [ ] GitHub Pages URL resolves and is reachable from a classroom-like network (tethered phone is fine).
- [ ] Every teammate has rehearsed their assigned slides at least twice with a stopwatch; total rehearsed runtime is under 25 minutes.

---

## 9. Source-of-Truth Map

Every slide cites real files and real commits. No fabrication.

**COBOL sources:**
- `src/main.cob` — menu dispatch, `COPY` directives → Slide 13
- `src/VIEWMESSAGE.cpy` paragraphs `7840-VIEW-MESSAGES`, `7841-VIEW-MESSAGES-LOOP` → Slides 12, 16
- `src/SENDMESSAGE.cpy` paragraphs `7820-VALIDATE-RECIPIENT`, `7830-WRITE-MESSAGE` → Slides 12, 17, 18
- `src/AUTH.cpy` paragraphs `4000-CREATE-ACCOUNT`, `4400-VALIDATE-PASSWORD` → Slides 05, 18
- `src/PROFILE.cpy` paragraph `7000-CREATE-EDIT-PROFILE` → Slide 06
- `src/SEARCH.cpy` paragraph `7500-FIND-SOMEONE-YOU-KNOW` → Slide 07
- `src/SENDREQ.cpy`, `src/VIEWREQ.cpy`, `src/NETWORK.cpy` → Slide 08
- `src/JOBS.cpy`, `src/BROWSEJOBS.cpy`, `src/APPLYJOB.cpy`, `src/VIEWAPPS.cpy` → Slides 09, 10, 11
- 24 copybooks + `main.cob` → Slides 13, 14

**Tests:**
- `tests/test_runner.py`, `tests/incollege_tests/runner.py` → Slide 20
- `tests/incollege_tests/preprocessing.py`, `tests/incollege_tests/persistence.py` → Slide 20
- `tests/fixtures/**` → Slide 19 (counts), Slides 05–12 (terminal playback source material)
- `tests/TEST_RUNNER_GUIDE.md`, `tests/JIRA_TASK_MAPPING.md` → Slide 19

**Process evidence (already packaged as Canvas submission):**
- `.dev/local/epic_9_submission/burndown_start.png` → Slide 22
- `.dev/local/epic_9_submission/burndown_end.png` → Slide 23
- `.dev/local/epic_9_submission/Jira.png` → Slide 21
- `.dev/local/epic_9_submission/Github.png` → Slide 21
- `.dev/local/epic_9_submission/Roles.txt` → Slide 02
- `README.md:477-485` → Slide 02

**Skills leveraged:**
- `.agents/skills/remotion-best-practices/` — `rules/animations.md`, `rules/compositions.md`, `rules/fonts.md`, `rules/assets.md`, `rules/text-animations.md`, `rules/charts.md`
- `.claude/skills/tailwind-v4-shadcn/` — Tailwind v4 setup reference
- `.claude/skills/frontend-design/` — distinctive UI patterns, spacing rhythm

---

## 10. Implementation Order

Each step is independently committable. Nothing outside `final_presentation/` is modified.

1. **Scaffold.** Copy the Canvas-submission assets from `.dev/local/epic_9_submission/` into `final_presentation/site/public/assets/`.
2. **Vite + React + Tailwind bootstrap.** `bun create vite` with the React-TS template, install Tailwind v4, configure `vite.config.ts` with `base` set for GitHub Pages.
3. **Install Remotion dependencies.** `remotion`, `@remotion/player`, `@remotion/google-fonts`, `@remotion/shapes`, `@remotion/transitions`, `shiki`, `zod`.
4. **Theme + globals.** Write `theme.ts`, `globals.css`, and the `<Wordmark />` component. Wire Google Fonts.
5. **Deck shell.** Write `Deck.tsx` with keyboard nav, URL sync, slide counter, fullscreen toggle. Stub slides 01–25 with placeholder titles so navigation works.
6. **Shared components.** `SlideFrame`, `PersonCard`, `StatCard`, `FeatureCallout`, `CodePanel`.
7. **Remotion compositions.** `Terminal`, `StatCounter`, `CodeReveal`, `ArchGraph`, `HarnessFlow`. Each is a Remotion composition embedded via `@remotion/player` inside the relevant slide.
8. **Data files.** Populate `src/data/` with team, stats, security, storage, the 3 verbatim COBOL snippets, and the 8 terminal playback scripts.
9. **Build slides in narrative order** (01 → 25), previewing each one in the dev server as you go.
10. **Shiki singleton.** Wire `src/shiki.ts` with a lazy-initialized highlighter preloaded with the `cobol` grammar and custom theme.
11. **GitHub Actions workflow** for Pages deploy. Push, verify the URL loads.
12. **`scripts/verify.sh`.** Diffs slide snippets against the real COBOL files; add it to the CI workflow so drift fails the build.
13. **Rehearse** with all 5 teammates at least twice, stopwatch running. Edit text-only data in `src/data/` based on timing.
14. **Paste the GitHub Pages URL into Canvas.** Done.

---

## 11. One-Screen Summary

- **What:** a 25-slide, dark-themed, React + Remotion-powered VC pitch website that tells the story of InCollege, demoes the 10 implemented features, tours the architecture, and closes with the sprint story.
- **Delivery:** hosted on GitHub Pages via a GitHub Actions workflow. Canvas receives the URL. Local `bun run preview` is the offline fallback if classroom wifi dies.
- **What's in the demos:** 8 animated terminal playbacks driven by recorded input/output pairs from `tests/fixtures/`. Real commands, real responses, frame-perfect timing.
- **Skipped intentionally:** the 5 rubric items that aren't in the codebase (pre-login search, English/Spanish, saved jobs, plus/standard tiers, system-wide notifications) — confirmed as outdated rubric content.
- **Everyone presents** — speaker-by-slide map in §5, every teammate gets the mic at least twice.
- **Every claim is backed** by a real file, commit, or Canvas-submission asset from `.dev/local/epic_9_submission/` — §9 lists them.
- **Nothing outside `final_presentation/` is modified** during implementation.

Sources consulted:
- [Remotion — @remotion/player](https://www.remotion.dev/docs/player)
- [Remotion — Passing props to a composition](https://www.remotion.dev/docs/passing-props)
- [Remotion — @remotion/google-fonts](https://www.remotion.dev/docs/google-fonts/)
- [Remotion — text animations (typewriter)](https://www.remotion.dev/docs/text-animations)
- [Shiki — syntax highlighter](https://shiki.style/)
- [shikijs/textmate-grammars-themes — COBOL grammar bundle](https://github.com/shikijs/textmate-grammars-themes)
- [Vite — GitHub Pages deployment](https://vitejs.dev/guide/static-deploy.html#github-pages)
- [Tailwind CSS v4](https://tailwindcss.com/blog/tailwindcss-v4)

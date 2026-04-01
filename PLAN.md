# InCollege COBOL Refactoring Plan

## Context

The InCollege application is a 2,254-line monolithic COBOL program (`src/main.cob`) that has grown through 8 epics of feature development. It now includes authentication, profiles, jobs, connections, messaging, and skills -- all in a single file with 10 copybooks for procedure code but zero separation of working storage or constants. Magic numbers are scattered throughout, boolean conventions are inconsistent, and the main file is difficult to navigate.

**Goal:** Break main.cob into a thin orchestrator (~200 lines) with well-organized copybooks, named constants, and domain-grouped working storage -- while preserving 100% behavioral compatibility verified by the existing 100+ test suite.

**Critical invariant:** All user I/O flows through INPUT.TXT -> 8100-READ-INPUT and 8000-WRITE-OUTPUT -> OUTPUT.TXT. This must never change. Tests compare OUTPUT.TXT line-by-line.

---

## Branch

`refactor/modularize` (from `impl/send_message`)

---

## Phase 1: Named Constants Copybook ✅ Lowest Risk

**What:** Create `src/WS-CONSTANTS.cpy` containing all magic numbers as named constants. Replace literals throughout main.cob and all existing copybooks.

**Why lowest risk:** Pure rename -- no logic change, no paragraph movement.

### Stream 1A: Create `src/WS-CONSTANTS.cpy`

```cobol
*> ============================================================
*> WS-CONSTANTS.cpy - Named constants for InCollege application
*> ============================================================
01  WS-CONST-MAX-ACCOUNTS          PIC 9     VALUE 5.
01  WS-CONST-MAX-PROFILES          PIC 9     VALUE 5.
01  WS-CONST-MAX-PENDING           PIC 99    VALUE 50.
01  WS-CONST-MAX-CONNECTIONS       PIC 99    VALUE 50.
01  WS-CONST-MAX-JOBS              PIC 999   VALUE 25.
01  WS-CONST-MAX-EXPERIENCES       PIC 9     VALUE 3.
01  WS-CONST-MAX-EDUCATIONS        PIC 9     VALUE 3.
01  WS-CONST-MAX-APPLICATIONS      PIC 999   VALUE 25.

*> File status codes
01  WS-CONST-FS-OK                 PIC XX    VALUE "00".
01  WS-CONST-FS-NOT-FOUND          PIC XX    VALUE "35".
01  WS-CONST-FS-OPEN-OK            PIC XX    VALUE "97".

*> Boolean flag constants
01  WS-CONST-YES                   PIC X     VALUE "Y".
01  WS-CONST-NO                    PIC X     VALUE "N".
01  WS-CONST-TRUE                  PIC 9     VALUE 1.
01  WS-CONST-FALSE                 PIC 9     VALUE 0.

*> Status sentinel values
01  WS-CONST-PENDING-STATUS        PIC X     VALUE "P".
01  WS-CONST-NONE-SALARY           PIC X(20) VALUE "NONE".
```

Add `COPY WS-CONSTANTS.` at the top of WORKING-STORAGE in main.cob.

Replace all magic numbers:
- `>= 5` (account limit, line 785) → `>= WS-CONST-MAX-ACCOUNTS`
- `WS-MAX-ACCOUNTS`, `WS-MAX-PENDING`, `WS-MAX-CONNECTIONS`, `WS-MAX-JOBS` → remove vars, use constants
- `"00"` / `"35"` / `"97"` file status checks → use named constants

### Checkpoint

```bash
cobc -x -free -I src -o bin/main src/main.cob && ./run_tests.sh
```

Commit: `refactor: extract named constants into WS-CONSTANTS.cpy`

---

## Phase 2: Working Storage Domain Copybooks ✅ Low Risk

**What:** Extract WORKING-STORAGE variable groups into 6 domain copybooks. main.cob's WORKING-STORAGE becomes a series of COPY statements.

**6 parallel streams (all independent):**

| Copybook | Variables |
|----------|-----------|
| `WS-IO-CONTROL.cpy` | File statuses, EOF flags, menu choices, output line, pushback buffer, program-running |
| `WS-ACCOUNTS.cpy` | WS-USER-ACCOUNTS, login vars, password validation vars |
| `WS-PROFILES.cpy` | WS-USER-PROFILES, profile temps, search vars, year validation |
| `WS-CONNECTIONS.cpy` | WS-PENDING-TABLE, WS-CONNECTIONS-TABLE, sendreq/viewreq/network vars |
| `WS-JOBS.cpy` | Job table, temp job vars, browse/app vars, display formatters |
| `WS-MESSAGES.cpy` | All WS-MSG-* variables |

Final WORKING-STORAGE in main.cob:
```cobol
WORKING-STORAGE SECTION.
    COPY WS-CONSTANTS.
    COPY WS-IO-CONTROL.
    COPY WS-ACCOUNTS.
    COPY WS-PROFILES.
    COPY WS-CONNECTIONS.
    COPY WS-JOBS.
    COPY WS-MESSAGES.
```

### Checkpoint

```bash
cobc -x -free -I src -o bin/main src/main.cob && ./run_tests.sh
```

Commit: `refactor: extract working storage into domain copybooks`

---

## Phase 3: Procedure Division Extraction ✅ Medium Risk

**What:** Move remaining paragraphs from main.cob into 7 new feature copybooks.

**What stays in main.cob (~200 lines):**
- IDENTIFICATION / ENVIRONMENT / FILE SECTION / WORKING-STORAGE (COPYs only)
- `0000-MAIN-PROGRAM`, `1000-INITIALIZE`, `2000-PROCESS-APPLICATION`, `5000-POST-LOGIN-MENU`
- `8000-WRITE-OUTPUT`, `8100-READ-INPUT`, `8105-PUSHBACK-INPUT`
- `9000-TERMINATE`
- All COPY statements

**7 parallel streams:**

| New Copybook | Paragraphs |
|-------------|-----------|
| `DATALOAD_SRC.cpy` | 1100-1162 (account/profile load), 9200-9275 (pending/conn/msg load), 5350/5360 (job/app load) |
| `AUTH_SRC.cpy` | 3000-3210 (login), 4000-4652 (account creation, password, persistence) |
| `PROFILE_SRC.cpy` | 7000-7400 (create/view/edit profile), 4650-4652 (profile persistence) |
| `SEARCH_SRC.cpy` | 7500-FIND-SOMEONE-YOU-KNOW and sub-paragraphs |
| `SKILLS_SRC.cpy` | 6000-SKILLS-MENU |
| `CONNMGMT_SRC.cpy` | 9300-9410 (write pending/connections), 7528-7529 (echo input helpers) |
| `NETWORK_SRC.cpy` | 7700-7710 (view network), 7800-MESSAGES-MENU dispatch |

### Dependency Note

`CONNMGMT_SRC.cpy` must be integrated before verifying `SENDREQ_SRC.cpy` and `VIEWREQ_SRC.cpy` (they PERFORM paragraphs that move there).

### Final COPY order in main.cob procedure division

```cobol
       COPY DATALOAD_SRC.
       COPY AUTH_SRC.
       COPY PROFILE_SRC.
       COPY SEARCH_SRC.
       COPY SKILLS_SRC.
       COPY CONNMGMT_SRC.
       COPY NETWORK_SRC.
       COPY SENDREQ_SRC.
       COPY JOBS_SRC.
       COPY SENDMESSAGE_SRC.
       COPY APPLYJOB_SRC.
       COPY VIEWAPPS_SRC.
       COPY VIEWREQ_SRC.
```

### Checkpoint

```bash
cobc -x -free -I src -o bin/main src/main.cob && ./run_tests.sh
```

Commit: `refactor: extract procedure paragraphs into feature copybooks`

---

## Phase 4: Cleanup ✅ Final Polish

- Delete `src/VIEWMESSAGES_SRC.cpy` and `src/JOBSEXT_SRC.cpy` (dead code, never COPY'd)
- Add 88-level conditions to all boolean flags for readability (additive, no logic change)
- Add standard header comments to each new copybook

### Checkpoint

```bash
cobc -x -free -I src -o bin/main src/main.cob && ./run_tests.sh
```

Commit: `refactor: cleanup dead code, add 88-levels and file headers`

---

## Final File Structure

```
src/
  main.cob              (~200 lines - thin orchestrator)
  WS-CONSTANTS.cpy      (named constants)
  WS-IO-CONTROL.cpy     (I/O flags, menu choices, output line)
  WS-ACCOUNTS.cpy       (account/login variables)
  WS-PROFILES.cpy       (profile variables + temps)
  WS-CONNECTIONS.cpy    (pending/connection variables)
  WS-JOBS.cpy           (job/application variables)
  WS-MESSAGES.cpy       (messaging variables)
  DATALOAD_SRC.cpy      (all file loading paragraphs)
  AUTH_SRC.cpy          (login/account creation)
  PROFILE_SRC.cpy       (profile CRUD)
  SEARCH_SRC.cpy        (find someone you know)
  SKILLS_SRC.cpy        (skills menu)
  CONNMGMT_SRC.cpy      (connection persistence)
  NETWORK_SRC.cpy       (view network + message menu dispatch)
  SENDREQ_SRC.cpy       (existing - send connection request)
  JOBS_SRC.cpy          (existing - job posting)
  BROWSEJOBS_SRC.cpy    (existing - job browsing, nested in JOBS_SRC)
  SENDMESSAGE_SRC.cpy   (existing - send message)
  APPLYJOB_SRC.cpy      (existing - apply for job)
  VIEWAPPS_SRC.cpy      (existing - view applications)
  VIEWREQ_SRC.cpy       (existing - view pending requests)
```

---

## Risk Mitigations

1. **Git tags** before each phase: `git tag pre-phase-N`
2. **One phase at a time** -- never start Phase N+1 until Phase N passes all tests
3. **8000-WRITE-OUTPUT / 8100-READ-INPUT / 8105-PUSHBACK-INPUT** NEVER leave main.cob
4. **No output changes** -- tests compare OUTPUT.TXT byte-for-byte

---

## Parallelization Guide

| Phase | Max Parallel Agents | Notes |
|-------|-------------------|-------|
| 1 | 3 | Agent 1: create cpy + main.cob top; Agent 2: main.cob bottom; Agent 3: existing .cpy files |
| 2 | 6 | Each agent owns one domain copybook |
| 3 | 7 | Each agent owns one feature copybook |
| 4 | 1-2 | Sequential cleanup |

**Rule:** After each phase, integrate all work, compile, run tests before starting next phase.

#!/usr/bin/env bash
# Verifies that the code snippets embedded in slide data match the real
# COBOL sources under src/. If any diff is non-empty, this script exits 1
# and the slides have drifted from reality.
#
# Run from the repo root or from final_presentation/.

set -euo pipefail

# Resolve repo root regardless of where the script is called from.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

SNIPPETS_DIR="$REPO_ROOT/final_presentation/site/src/data/snippets"
SRC_DIR="$REPO_ROOT/src"

fail=0

check() {
  local label="$1"
  local cob_file="$2"
  local start="$3"
  local end="$4"
  local snippet="$5"

  if [ ! -f "$SRC_DIR/$cob_file" ]; then
    printf '  [skip] %-30s — source %s not found\n' "$label" "$cob_file"
    return
  fi
  if [ ! -f "$SNIPPETS_DIR/$snippet" ]; then
    printf '  [skip] %-30s — snippet %s not found\n' "$label" "$snippet"
    return
  fi

  if diff -u \
      <(sed -n "${start},${end}p" "$SRC_DIR/$cob_file") \
      "$SNIPPETS_DIR/$snippet" > /dev/null; then
    printf '  [ok]   %-30s (lines %s-%s of %s)\n' "$label" "$start" "$end" "$cob_file"
  else
    printf '  [FAIL] %-30s (lines %s-%s of %s)\n' "$label" "$start" "$end" "$cob_file"
    diff -u \
      <(sed -n "${start},${end}p" "$SRC_DIR/$cob_file") \
      "$SNIPPETS_DIR/$snippet" || true
    fail=1
  fi
}

echo "Verifying presentation snippets against src/ …"
check "Recursive view loop"       "VIEWMESSAGE.cpy" 75  120 "view-loop.cob"
check "Bidirectional connection"  "SENDMESSAGE.cpy" 198 225 "bidirectional.cob"
check "Password validation"       "AUTH.cpy"        230 275 "password-policy.cob"

if [ "$fail" -ne 0 ]; then
  echo ""
  echo "FAIL: one or more snippets have drifted from the real COBOL sources."
  exit 1
fi

echo ""
echo "All snippets match their source files. ✓"

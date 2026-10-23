#!/usr/bin/env bash
# order-phases.sh against a stub `gh`: `one` only for a known change off main touching no order path; `both` on main
# and on every failure, ambiguity or order path -- it never drops a row it cannot prove it may.
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
printf '\033[36m▸\033[0m order-phases.sh\n'
stub="$(mktemp -d)"
trap 'rm -rf "$stub"' EXIT
# STUB_FILES: the changed files (one a line); STUB_GH=fail fails the read, =hang never answers, =nolist answers no list.
cat > "$stub/gh" <<'STUB'
#!/usr/bin/env bash
echo "$*" >> "$STUB_LOG"
case "${STUB_GH:-}" in
  fail) echo "HTTP 502" >&2; exit 1 ;;
  hang) exec sleep 86400 ;;
  nolist) exit 5 ;;  # what gh's --jq exits when the error() above fires
esac
printf '%s' "${STUB_FILES:-}" | sed '/^$/d'
STUB
chmod +x "$stub/gh"
export STUB_LOG="$stub/log"
phases() { : > "$STUB_LOG"; PATH="$stub:$PATH" GH_REPO=o/r ORDER_PHASES_TIMEOUT=2 bash "$REPO_ROOT/scripts/ci/order-phases.sh" "$@" 2> "$stub/err"; }

check "main runs both, whatever changed" "both" "$(STUB_FILES=README.md phases refs/heads/main abc)"
check "...without asking what changed" "0" "$(wc -l < "$STUB_LOG" | tr -d ' ')"
check "a branch changing only docs and scripts runs one" "one" "$(STUB_FILES=$'docs/x.md\nscripts/identity-measure' phases refs/heads/agent abc)"
check "...comparing its commit with main" "api repos/o/r/compare/main...abc" "$(cut -d" " -f1-2 "$STUB_LOG")"
check "a branch touching the resolver runs both" "both" "$(STUB_FILES=$'docs/x.md\nidentity/src/main/scala/A.scala' phases refs/heads/agent abc)"
check "...saying which file" "1" "$(grep -c 'identity/src/main/scala/A.scala is order code' "$stub/err")"
for f in common/src/main/x worker/src/main/x scrapers/src/main/x e2e/src/test/scala/services/movies/GermanyConvergenceSpec.scala \
         build.sbt project/plugins.sbt .github/workflows/country-convergence-leg.yml .github/order-independence-paths.txt scripts/ci/order-phases.sh; do
  check "a branch touching $f runs both" "both" "$(STUB_FILES=$f phases refs/heads/agent abc)"
done
check "a change that cannot be read runs both" "both" "$(STUB_GH=fail phases refs/heads/agent abc)"
check "a read that hangs is cut off, and runs both" "both" "$(STUB_GH=hang phases refs/heads/agent abc)"
check "an answer with no file list runs both" "both" "$(STUB_GH=nolist phases refs/heads/agent abc)"
check "a list that may be cut short (100 files) runs both" "both" "$(STUB_FILES=$(seq -f 'docs/%g.md' 100) phases refs/heads/agent abc)"
check "...99 docs files are a whole list" "one" "$(STUB_FILES=$(seq -f 'docs/%g.md' 99) phases refs/heads/agent abc)"
check "no commit runs both" "both" "$(phases refs/heads/agent '')"
check "no repository runs both" "both" "$(PATH="$stub:$PATH" GH_REPO='' bash "$REPO_ROOT/scripts/ci/order-phases.sh" refs/heads/agent abc 2>/dev/null)"
check "an unreadable path list runs both" "both" "$(STUB_FILES=docs/x.md ORDER_PATHS=/nonexistent phases refs/heads/agent abc)"
check "a branch identical to main (no files) runs one" "one" "$(STUB_FILES='' phases refs/heads/agent abc)"
spec_summary

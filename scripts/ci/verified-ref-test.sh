#!/usr/bin/env bash
# scripts/ci/verified-ref.sh over a scratch repository and its bare origin: a row's last green base, kept as a marker
# under refs/verified/<name>; advanced only forward; and, for main.yml's old-ref compile, a tool change in a push whose
# queued Main run was replaced still in the next run's range (2026-11-21: the push's `before` left it out).
# Run: bash scripts/ci/verified-ref-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m verified-ref.sh\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
repo="$work/repo" origin="$work/origin.git"
scratch_repo "$repo" -b main
scratch_repo "$origin" --bare -b main
mkdir -p "$repo/scripts/ci"
cp "$REPO_ROOT/scripts/ci/verified-ref.sh" "$repo/scripts/ci/"
commit() { # <path> <message>
    mkdir -p "$repo/$(dirname "$1")"; echo "$2" >> "$repo/$1"
    scratch_git "$repo" add -A; scratch_git "$repo" commit -qm "$2"; scratch_git "$repo" rev-parse HEAD
}
ungit() { local v; for v in $(compgen -e | grep '^GIT_'); do unset "$v"; done; }
vref() { (cd "$repo" && ungit; bash scripts/ci/verified-ref.sh "$@" 2> "$work/err"); }
code() { (cd "$repo" && ungit; bash scripts/ci/verified-ref.sh "$@" > /dev/null 2>&1; echo $?); }
a=$(commit docs/a.md "the base")
scratch_git "$repo" remote add origin "$origin"; scratch_git "$repo" push -q origin main

check "no marker yet: no base, said by nothing (the caller falls back to the push's before)" "|0" "$(vref base old-ref-compile)|$(code base old-ref-compile)"
check "a green run on main advances the marker to HEAD" "$a" "$(vref advance old-ref-compile > /dev/null; vref base old-ref-compile)"
check "...as a marker commit, never main's own (GitHub refuses a ref update over a workflow change, Main #2301)" "1" \
  "$(scratch_git "$origin" cat-file -e refs/verified/old-ref-compile:verified && echo 1)"

# THE REPLACED RUN: push B changes a tool file, and its queued Main run is replaced by push C's before it starts -- no
# compile ran with B, so the marker stays at A. C's own `before` is B: diffed from there, B's tool change is not seen.
b=$(commit worker/src/test/scala/scripts/IdentitySlice.scala "a tool change")
c=$(commit docs/c.md "a docs change")
scratch_git "$repo" push -q origin main
patterns="$work/patterns"
awk '/id: slice-tools/{on=1} on&&/patterns: \|/{p=1; next} p&&/^ *$/{exit} p{sub(/^ +/,""); print}' \
  "$REPO_ROOT/.github/workflows/main.yml" > "$patterns"
matches() { scratch_git "$repo" diff --name-only "$1" "$c" | bash "$REPO_ROOT/.github/actions/changed-paths/matches.sh" "$patterns"; }
check "main.yml's old-ref gate lists the tool file (the case reads the real patterns)" "1" "$(grep -cx 'worker/src/test/scala/scripts/IdentitySlice.scala' "$patterns")"
check "a replaced run's push: the marker still records the last green compile" "$a" "$(vref base old-ref-compile)"
check "...so the next run's range from it holds the replaced push's tool change: the compile runs" "true" "$(matches "$(vref base old-ref-compile)")"
check "...where the push's own before (the replaced push) would have skipped it" "false" "$(matches "$b")"

check "a later green run advances it past both" "$c" "$(vref advance old-ref-compile > /dev/null; vref base old-ref-compile)"
scratch_git "$repo" checkout -q "$b"
check "...and a slower run of an older commit never moves it back" "0|$c" \
  "$(code advance old-ref-compile)|$(vref base old-ref-compile)"
scratch_git "$repo" checkout -q main
check "an unknown command is refused" "2" "$(code everything old-ref-compile)"
scratch_git "$repo" remote set-url origin "$work/no-such-origin"
check "an origin that cannot be read is no base (exit 2), never an empty one" "2" "$(code base old-ref-compile)"
check "...and no advance (exit 2): the next run would judge from a stale base" "2" "$(code advance old-ref-compile)"

spec_summary

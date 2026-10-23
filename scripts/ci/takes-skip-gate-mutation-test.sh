#!/usr/bin/env bash
# THE GATE FAILS CLOSED, proved by mutation: each mutant below is takes-skip-gate.sh with ONE check taken out, and
# takes-skip-gate-test.sh must FAIL against every one -- a check whose removal no test notices is a check that could
# silently stop working and let a takes row be skipped that should run. Each mutant is applied by exact text; a mutant
# whose text is no longer in the gate fails this test too (the gate changed: re-aim the mutant at the new check).
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
gate="$REPO_ROOT/scripts/ci/takes-skip-gate.sh"
printf '\033[36m▸\033[0m takes-skip-gate.sh, mutated\n'
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# <name> @@ <exact text in the gate> @@ <what replaces it>
mutants=(
  'the mode is not checked @@ *) run "TAKES_SKIP_MODE is @@ *) : "TAKES_SKIP_MODE is'
  'the proof command is not required @@ [ "$#" -ge 5 ] && [ "$4" = -- ] || run @@ true || run'
  'commit names are not checked @@ [[ "$base" =~ ^[0-9a-f]{40}$ ]] && [[ "$head" =~ ^[0-9a-f]{40}$ ]] || run @@ true || run'
  'one commit is allowed @@ [ "$base" != "$head" ] || run @@ true || run'
  'a failed diff reads as no change @@ "$head" --) || run "git could not diff @@ "$head" --) || true "git could not diff'
  'an empty change is allowed @@ [ -n "$changed" ] || run @@ true || run'
  'any file is allowed @@ inert "$file" || run @@ inert "$file" || true'
  'the placeholders count as a title rule @@ TitleRulePlaceholders.scala) return 1 ;; @@ TitleRulePlaceholders.scala) return 0 ;;'
  'no rule change is needed @@ [ "$rules" -gt 0 ] || run @@ true || run'
  'identity code is not denied @@ [ -z "$denied" ] || run @@ true || run'
  'changed tooling is allowed @@ [ -z "$tooling_diff" ] || run @@ true || run'
  'a missing trace is allowed @@ [ -s "$trace" ] || run @@ true || run'
  'an empty trace is allowed @@ [ "$lines" -gt 0 ] || run @@ true || run'
  'the proof exit is not read @@ [ "$rc" = 0 ] || run @@ true || run'
  'the proof line is not read @@   || run "the proof said no @@   || true "the proof said no'
  'a proof of nothing is allowed @@ [ "$readings" -gt 0 ] || run @@ true || run'
  'a differ row is ignored @@ [ ! -s "$out/differ.tsv" ] || run @@ true || run'
  'the proof is unbounded @@ timeout "${TAKES_SKIP_PROOF_TIMEOUT:-900}" "$@" @@ "$@"'
)

# each mutant made, then the test run against all of them at once (four at a time): a mutant the test does not fail on
# is a check nothing guards
mkdir -p "$work/mutants"
i=0
for m in "${mutants[@]}"; do
  i=$((i + 1))
  name=${m%% @@ *}; rest=${m#* @@ }; from=${rest%% @@ *}; to=${rest#* @@ }
  printf '%s' "$name" > "$work/mutants/$i.name"
  python3 -I - "$gate" "$work/mutants/$i.sh" "$from" "$to" <<'PY'
import sys
src, dst, frm, to = sys.argv[1:]
text = open(src).read()
n = text.count(frm)
open(dst, "w").write(text.replace(frm, to, 1) if n == 1 else "exit 99 # mutant text not found exactly once\n")
sys.exit(0 if n == 1 else 1)
PY
  check "the mutant '$name' is aimed at exactly one check in the gate" 0 "$?"
done
# shellcheck disable=SC2016 # expanded by the inner bash
seq 1 "$i" | xargs -P 4 -I{} bash -c 'SPEC_HANG_BOUND=20 TAKES_SKIP_GATE_UNDER_TEST="$1/mutants/$2.sh" bash "$3" > "$1/mutants/$2.out" 2>&1; echo $? > "$1/mutants/$2.rc"' \
  _ "$work" {} "$REPO_ROOT/scripts/ci/takes-skip-gate-test.sh"
for n in $(seq 1 "$i"); do
  check "takes-skip-gate-test.sh fails against the mutant: $(cat "$work/mutants/$n.name")" 1 \
    "$([ "$(cat "$work/mutants/$n.rc" 2> /dev/null)" != 0 ] && grep -q '✗' "$work/mutants/$n.out" && echo 1 || echo 0)"
done
# ...and passes against the gate itself, so the failures above are the mutants'
bash "$REPO_ROOT/scripts/ci/takes-skip-gate-test.sh" > "$work/out" 2>&1
check "takes-skip-gate-test.sh passes against the gate itself" 0 "$?"
spec_summary

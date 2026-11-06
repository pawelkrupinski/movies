#!/usr/bin/env bash
# drift-replays-compare.sh: two replays of the same seeds (the drift harness's A/A, two `takes-drift-aa-pass<N>` rows,
# or identity-drift-check.sh's DRIFT_TWICE) are one only when their drift and flips TSVs hold the same lines in any
# order:
#   - alike (rows reordered): 0, `deterministic`;
#   - a line apart in either TSV: 1, a `not deterministic` verdict naming the line count, the lines in the log;
#   - a side missing a TSV: 2, no verdict.
# Run: bash scripts/ci/drift-replays-compare-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m drift-replays-compare.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
side() { # <dir> <drift lines> <flips lines>
  mkdir -p "$1"; printf '%b' "$2" > "$1/drift-pl.tsv"; printf '%b' "$3" > "$1/flips-pl.tsv"
}
compare() { bash "$REPO_ROOT/scripts/ci/drift-replays-compare.sh" pl "$work/a" "$work/b" "$work/verdict.txt" > "$work/stdout" 2>&1; echo $?; }

side "$work/a" 'seed\tvenue\n1\tA\n1\tB\n' 'seed\tvenue\n1\tX\n'
side "$work/b" 'seed\tvenue\n1\tB\n1\tA\n' 'seed\tvenue\n1\tX\n'
check "two replays alike, rows in another order, pass" "0" "$(compare)"
check "...said deterministic" "1" "$(grep -c 'deterministic -- a second replay of the same seed(s) wrote the same TSVs' "$work/stdout")"

side "$work/b" 'seed\tvenue\n1\tB\n1\tA\n' 'seed\tvenue\n1\tY\n'
check "a flip apart fails" "1" "$(compare)"
check "...the verdict counting the lines apart" "1" "$(grep -c '^not deterministic: two replays of the same seed(s) differ in 2 TSV line(s)' "$work/verdict.txt")"
check "...the lines in the log" "1" "$(grep -c '^> 1	Y' "$work/stdout")"

rm "$work/b/drift-pl.tsv"
check "a side with no drift TSV is no verdict" "2" "$(compare)"

spec_summary

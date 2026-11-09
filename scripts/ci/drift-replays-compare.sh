#!/usr/bin/env bash
# TWO REPLAYS OF THE SAME SEEDS ARE ONE (the drift harness's determinism): <cc> <dir A> <dir B> <verdict file>.
# Each dir holds a replay's drift-<cc>.tsv and flips-<cc>.tsv (scripts.IdentityDrift's --out). Alike in any line order:
# exit 0. A line apart in either: exit 1, the lines in the log and a `not deterministic` verdict in <verdict file>
# (harness_aa.py judge reads it: the harness's own fault). A side missing a TSV: exit 2, nothing judged.
# Its callers: identity-drift-check.sh's DRIFT_TWICE (both replays in one row, the daily full run) and the leg's
# `takes-drift-aa` comparison of the A/A rows' two passes (identity-takes.yml `takes-drift-aa`).
# Tested by drift-replays-compare-test.sh.
set -uo pipefail

cc=${1:?country code} a=${2:?first replay dir} b=${3:?second replay dir} verdict=${4:?verdict file}
for d in "$a" "$b"; do
  [ -f "$d/drift-$cc.tsv" ] && [ -f "$d/flips-$cc.tsv" ] ||
    { echo "identity drift ($cc): the replay in $d wrote no drift-$cc.tsv / flips-$cc.tsv -- nothing compared" >&2; exit 2; }
done
differ=0
for f in "drift-$cc.tsv" "flips-$cc.tsv"; do
  if ! cmp -s <(sort "$a/$f") <(sort "$b/$f"); then
    differ=$((differ + $(diff <(sort "$a/$f") <(sort "$b/$f") | grep -c '^[<>]')))
    echo "identity drift ($cc): NOT DETERMINISTIC -- $f differs between two replays of the same seed(s):"
    diff <(sort "$a/$f") <(sort "$b/$f") | grep '^[<>]' | head -20
  fi
done
if [ "$differ" -gt 0 ]; then
  echo "not deterministic: two replays of the same seed(s) differ in $differ TSV line(s) -- the harness or the code reads something besides its inputs" > "$verdict"
  exit 1
fi
echo "identity drift ($cc): deterministic -- a second replay of the same seed(s) wrote the same TSVs"

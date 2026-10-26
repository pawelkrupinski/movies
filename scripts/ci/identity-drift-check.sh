#!/usr/bin/env bash
# THE INCREMENTAL = WHOLE GATE, for one country in its hermetic `takes-drift-<seed>` rows: replay production's incremental
# identity sequence (scripts.IdentityDrift: boot with answers missing, ticks, new scrapes, re-scrapes, listings leaving
# and returning, deploys with and without a rules change, through the stores' own BSON) over the pinned recording the
# row laid, and fail on any listing whose take at a checkpoint differs from a whole resolve of the same listings --
# unless test/resources/fixtures/identity-drift-allow.tsv names it, with its reason.
#
# One commit's verdict, not base vs head: incremental = whole is a property every commit must hold, so a red here is
# this code's (or a recording's that exposes it -- fix the code, or allow-list the listing with why it is intended).
#
# usage: identity-drift-check.sh <cc> <out dir>
#   env: HEAP (sbt -Xmx, default 6g), DRIFT_SEEDS (default 2), DRIFT_SEED (that one seed alone, in place of DRIFT_SEEDS:
#        a leg's `takes-drift-<seed>` row each replays one, side by side -- in turn the two were 20-54 minutes of the
#        takes row, run 2809), GITHUB_STEP_SUMMARY (optional)
# Writes <out>/drift-<cc>.tsv (every divergence, allowed ones included) and <out>/report.txt.
# Exit: 0 none beyond the allow-list, 1 divergences, 2 it could not run.
set -uo pipefail

cc=${1:?country code}
out=${2:?out dir}
heap=${HEAP:-6g}
seeds=(--seeds "${DRIFT_SEEDS:-2}")
[ -n "${DRIFT_SEED:-}" ] && seeds=(--seed "$DRIFT_SEED")
here=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
fixtures="$here/test/resources/fixtures"
mkdir -p "$out"

[ -f "$fixtures/corpus/cinema-scrapes-$cc.json.gz" ] || { echo "identity drift ($cc): no recorded corpus in $fixtures/corpus" >&2; exit 2; }
code=0
( cd "$here" && sbt -J-Xmx"$heap" 'set worker / Test / run / fork := false' \
    "worker/Test/runMain scripts.IdentityDrift $cc --recording $fixtures --families $fixtures/identity-unmatched/$cc.json.gz \
     --allow $fixtures/identity-drift-allow.tsv --labels $fixtures/identity-judgements.tsv ${seeds[*]} --out $out" ) \
  > "$out/sbt.log" 2>&1 || code=$?
grep -E '^## |^  [0-9]+ listing|^      ' "$out/sbt.log" > "$out/report.txt"
[ -f "$out/drift-$cc.tsv" ] || { tail -40 "$out/sbt.log" >&2; echo "identity drift ($cc): the replay wrote nothing" >&2; exit 2; }
cat "$out/report.txt"
if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
  { echo "### identity drift ($cc): incremental vs whole on the pinned recording"; echo '```'; cat "$out/report.txt"; echo '```'; } >> "$GITHUB_STEP_SUMMARY"
fi
# The verdict is the report's own line (`## <cc>: … N divergence(s) … M allow-listed`): sbt's exit cannot tell the tool's
# verdict (sys.exit 1) from a failed build.
summary=$(grep -m1 '^## ' "$out/report.txt")
[ -n "$summary" ] || { tail -40 "$out/sbt.log" >&2; echo "identity drift ($cc): no verdict (sbt exit $code)" >&2; exit 2; }
found=$(sed -E 's/.* ([0-9]+) divergence\(s\).*/\1/' <<< "$summary")
allowed=$(sed -E 's/.* ([0-9]+) allow-listed.*/\1/' <<< "$summary")
[ $((found - allowed)) -gt 0 ] && exit 1
exit 0

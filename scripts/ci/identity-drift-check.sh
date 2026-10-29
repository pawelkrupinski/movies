#!/usr/bin/env bash
# THE INCREMENTAL = WHOLE GATE, for one country in its hermetic `takes-drift-<seed>` rows: replay production's incremental
# identity sequence (scripts.IdentityDrift: boot with answers missing, ticks, new scrapes, re-scrapes, listings leaving
# and returning, deploys with and without a rules change, through the stores' own BSON) over the pinned recording the
# row laid, and fail on any listing whose take at a checkpoint differs from a whole resolve of the same listings --
# unless test/resources/fixtures/identity-drift-allow.tsv names it, with its reason. And on any FLIP: a listing whose served
# take, recorded at every projection of the replay, goes back to a film it left, changes more than once, or changes where
# none of its deciding inputs did (services.identity.TakeFlips: prod's `kinowo_identity_listing_flips`, which an end-state
# comparison cannot see) -- unless test/resources/fixtures/identity-flips-allow.tsv names it, and that list stays empty.
#
# A RED HERE is fixed in the incremental state that diverges (a lag, a budget, queue order, a cache, a restart key), never
# by a new resolver rule (the owner, 2026-10-29: a rule is identity design, measured on the full corpus); and it is not
# closed until its listing's drift slice is committed (scripts/identity-slice <cc> SELECTOR --emit-drift-slice, replayed by
# IdentityDriftSlicesSpec). docs/design/identity-no-drift.md, "Fixing a drift or flip red"; pre-push's driftguards.
#
# One commit's verdict, not base vs head: incremental = whole is a property every commit must hold, so a red here is
# this code's (or a recording's that exposes it -- fix the code, or allow-list the listing with why it is intended).
#
# usage: identity-drift-check.sh <cc> <out dir>
#   env: HEAP (sbt -Xmx, default 6g), DRIFT_SEEDS (default 2), DRIFT_SEED (that one seed alone, in place of DRIFT_SEEDS:
#        a leg's `takes-drift-<seed>` row each replays one, side by side -- in turn the two were 20-54 minutes of the
#        takes row, run 2809), GITHUB_STEP_SUMMARY (optional)
# Writes <out>/drift-<cc>.tsv (every divergence, allowed ones included), <out>/flips-<cc>.tsv (every flip),
# <out>/report.txt, and <out>/verdict.txt: one line naming what the replay found beyond the allow-lists, divergences and
# flips apart -- what the row's ::error says (a step summary is discarded on Forgejo; and run 3509's rows failed on flips
# alone, 0 divergences, under a title saying the state "ends elsewhere than a whole resolve", which three fixers chased).
# KNOWN FLIPS (test/resources/fixtures/identity-flips-baseline.tsv) pass, each run saying "N known flip(s) remaining" as a
# ::warning and in the step summary, and naming a baselined flip that is gone "remove its row"; a NEW flip fails. The
# baseline only shrinks; empty, every flip fails. Divergences have no baseline.
# Exit: 0 none beyond the allow-lists and the known flips, 1 divergences or new flips, 2 it could not run.
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
     --allow $fixtures/identity-drift-allow.tsv --flips-allow $fixtures/identity-flips-allow.tsv \
     --flips-baseline $fixtures/identity-flips-baseline.tsv --labels $fixtures/identity-judgements.tsv ${seeds[*]} --out $out" ) \
  > "$out/sbt.log" 2>&1 || code=$?
grep -E '^## |^  [0-9]+ listing|^      ' "$out/sbt.log" > "$out/report.txt"
[ -f "$out/drift-$cc.tsv" ] || { tail -40 "$out/sbt.log" >&2; echo "identity drift ($cc): the replay wrote nothing" >&2; exit 2; }
cat "$out/report.txt"
if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
  { echo "### identity drift ($cc): incremental vs whole on the pinned recording"; echo '```'; cat "$out/report.txt"; echo '```'; } >> "$GITHUB_STEP_SUMMARY"
fi
# The verdict is the report's own line (`## <cc>: … N divergence(s) … M allow-listed`): sbt's exit cannot tell the tool's
# verdict (sys.exit 1) from a failed build.
summary=$(grep -m1 -E '^## [a-z]+: ' "$out/report.txt")
[ -n "$summary" ] || { tail -40 "$out/sbt.log" >&2; echo "identity drift ($cc): no verdict (sbt exit $code)" >&2; exit 2; }
found=$(sed -E 's/.* ([0-9]+) divergence\(s\).*/\1/' <<< "$summary")
allowed=$(sed -E 's/.* ([0-9]+) allow-listed.*/\1/' <<< "$summary")
# The flips' own line (`## <cc> flips: N flip(s) … M allow-listed`), printed once the replay ran.
flips=$(grep -m1 -E '^## [a-z]+ flips: ' "$out/report.txt")
[ -n "$flips" ] || { tail -40 "$out/sbt.log" >&2; echo "identity drift ($cc): no flips verdict (sbt exit $code)" >&2; exit 2; }
flipped=$(sed -E 's/.* ([0-9]+) flip\(s\).*/\1/' <<< "$flips")
flips_allowed=$(sed -E 's/.* ([0-9]+) allow-listed.*/\1/' <<< "$flips")
# THE KNOWN-FLIPS BASELINE (identity-flips-baseline.tsv; the owner, 2026-10-29: "fail build only on new flips, focus in
# eliminating all old flips and then fail on all flips"): its flips pass, said loudly; a row whose listing no longer flips
# is named fixed. Empty, every flip fails. A replay from before the baseline prints no `known` count: none known.
known=$(sed -nE 's/.* ([0-9]+) known$/\1/p' <<< "$flips"); known=${known:-0}
known_line=$(grep -m1 -E '^## [a-z]+ known flips: ' "$out/report.txt" || true)
if [ "$known" -gt 0 ]; then
  echo "::warning title=identity drift ($cc)::$known known flip(s) remaining (identity-flips-baseline.tsv) -- not failed, to be fixed: ${known_line#\#\# }"
fi
while IFS= read -r fixed; do
  [ -n "$fixed" ] && echo "::warning title=identity drift ($cc): a baselined flip is gone::${fixed#"${fixed%%[! ]*}"} -- remove its row from identity-flips-baseline.tsv"
done < <(grep -E '^      FIXED, remove its row: ' "$out/report.txt" || true)
if [ -n "${GITHUB_STEP_SUMMARY:-}" ] && [ -n "$known_line" ]; then
  { echo "**${known_line#\#\# }** -- known flips pass until fixed; every other flip fails"; } >> "$GITHUB_STEP_SUMMARY"
fi
open_divergences=$((found - allowed))
open_flips=$((flipped - flips_allowed - known))
echo "$open_divergences divergence(s) from a whole resolve and $open_flips flip(s) on the way, beyond the allow-lists and $known known flip(s)" > "$out/verdict.txt"
[ "$open_divergences" -gt 0 ] && exit 1
[ "$open_flips" -gt 0 ] && exit 1
exit 0

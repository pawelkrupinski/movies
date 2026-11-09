#!/usr/bin/env bash
# THE INCREMENTAL = WHOLE GATE, for one country in its hermetic `takes-drift-1` / `takes-drift-2` rows: replay
# production's incremental identity sequence (scripts.IdentityDrift: boot with answers missing, ticks, new scrapes,
# re-scrapes, listings leaving and returning, deploys with and without a rules change, through the stores' own BSON)
# over the pinned recording the row laid, on the row's FIXED seeds (test/resources/fixtures/drift-gate-seeds.tsv,
# scripts/ci/drift_seeds.py), and judge two things:
#
#   1. DIVERGENCES -- a listing whose take at a checkpoint differs from a whole resolve of the same listings -- fail
#      the head outright, whatever its base did, unless test/resources/fixtures/identity-drift-allow.tsv names it with
#      its reason: incremental = whole is a correctness property every commit must hold, not a flip.
#   2. FLIPS -- a listing whose served take, recorded at every projection of the replay, goes back to a film it left,
#      changes more than once, or changes where none of its deciding inputs did (services.identity.TakeFlips: prod's
#      `kinowo_identity_listing_flips`) -- are judged DIFFERENTIALLY (the owner, 2026-11-04: "a gate that only fails on
#      what the change adds"; docs/design/identity-no-drift.md, "The differential gate"): the row replays the same seeds
#      over the same recording at the change's BASE, and fails only on a flip the head shows and the base does not
#      (scripts/ci/drift_diff.py). A flip both show is PRE-EXISTING: printed as a `pre-existing (backlog)` line and never
#      failed -- it is the backlog's (test/resources/fixtures/identity-flip-backlog.tsv), burnt down by its owner, and
#      found by the nightly explorer (.github/workflows/identity-drift-explorer.yml), not a red on whichever commit
#      replays its seed. A newborn check's TEMPORARY rows on identity-flips-allow.tsv still pass its own head-only flips.
#
# THE BASE, per seed: the newest first-parent ancestor (DRIFT_BASE_DEPTH, 60) whose result for that seed main STORED
# GREEN -- so a flip a red commit added stays the change's until it is fixed, never absorbed by the next commit --
# else the parent (on main) or the merge-base with main (a branch), resolved here. DRIFT_BASE names one outright.
# A base whose IDENTITY CODE (the paths that can move a take, and the drift harness: code_of below) is the head's has,
# over the same recording and seed, the head's flips: nothing is replayed twice, every flip is pre-existing -- so a
# commit touching no identity code can never be red on flips.
# THE STORE (DRIFT_STORE, a release; empty: nothing stored or read): each green head on main stores each seed's flips
# as `drift-green-<cc>-<seed>-<key>.tsv.gz`, and a base resolved here as `drift-calc-...` (any ref: the key names the
# code, the recording and the seed, never the ref); the walk above reads only the green ones.
# THE HEAD'S REPLAY IS A FUNCTION OF ITS INPUTS (DRIFT_TWICE proves it daily): stored as `drift-head-<cc>-<seeds>-<key>`,
# its key everything the replay can read -- the tree of every module on worker/Test's classpath (all of each module's
# src/, not code_of's src/main), the build, test/ (the recording's pins, the allow-lists, the judgements, the backlog),
# this check and its helpers, the recording, the JDK and the heap -- a head whose inputs a run already replayed reads
# that replay instead of running it again (8 of 10 of main's runs, 2026-11-08..09: commits outside the identity code;
# a UK row 13 of its 16 minutes). Any input moved replays. Never read: DRIFT_TWICE (it exists to replay), the
# explorer, DRIFT_FRESH=true (a profiled run), and no DRIFT_STORE (the A/A passes): those always replay.
#
# THE SCENARIO GUARD: each replay writes what its harness DREW per seed (`scenario-<cc>.tsv`: the venue arrangement and
# every fill draw, scripts.IdentityDrift.Scenario), and the base's is kept beside its flips (`drift-greenscenario-...`,
# `drift-calcscenario-...`). Before any flip is judged, head and base must have met the same scenario on each seed --
# the arrangement over the same listings, the draw of every question both ask (scripts/ci/drift_scenario.py): else the
# row is a HARNESS ERROR (exit 2, no verdict on flips), loudly, never flips. For two weeks one `Random` drew the fills and
# the halves, so a change asking one question more replayed another scenario than its base, unseen (3e97bde52). A base
# stored before the guard has no scenario: said, not compared.
#
# A RED HERE is fixed in the incremental state that diverges (a lag, a budget, queue order, a cache, a restart key), never
# by a new resolver rule (the owner, 2026-10-29: a rule is identity design, measured on the full corpus).
# docs/design/identity-no-drift.md, "Fixing a drift or flip red"; pre-push's driftguards.
#
# usage: identity-drift-check.sh <cc> <out dir>
#   env: DRIFT_SEED_LIST (the row's seeds, space- or comma-separated: drift_seeds.py seeds), DRIFT_SEED (one seed),
#        DRIFT_SEEDS (seeds 1..N; the default, 2, when neither is set); DRIFT_BASE, DRIFT_BASE_DEPTH, DRIFT_STORE (above);
#        DRIFT_TWICE=true (the daily full run's, convergence-failures 2026-10-29 G10): replay the head's seeds a second
#        time into <out>/again and fail when the two runs' drift and flips TSVs differ -- the replay must be a function of
#        its inputs; HEAP (sbt -Xmx, default 6g); GITHUB_STEP_SUMMARY (optional); GH_TOKEN for the store.
#        DRIFT_EXPLORE (the explorer: the head alone, never red, <out>/explore-<cc>.tsv; DRIFT_EXPLORE_REF names the output).
# Writes <out>/drift-<cc>.tsv and <out>/flips-<cc>.tsv (the head's, every seed), <out>/base/ (each seed's base flips
# and how it was had), <out>/diff.txt (the differential, per seed), <out>/new-<seed>.tsv (the flips the change adds),
# <out>/seed (the seeds replayed, a line each, a red one first -- scripts/identity-drift-ci pins a red's seed from it),
# <out>/reproduce.txt (the red's one-line local reproducer, and the drift slice of each red listing that has one),
# <out>/report.txt (the head's report) and <out>/verdict.txt: one line naming the divergences and the flips the change
# adds apart (agentctl and the ::error read it: "... N flip(s) on the way ..."; a scenario mismatch: "harness error: ...");
# <out>/scenario-<cc>.tsv (the head's scenario), <out>/base/scenario-<seed>.tsv (the base's, where it has one) and
# <out>/scenario.txt (their comparison, per seed).
# Exit: 0 no divergence and no flip the change adds, 1 some, 2 it could not run or head and base met different scenarios
# (no verdict). Tested by
# identity-drift-check-test.sh.

set -uo pipefail

cc=${1:?country code}
out=${2:?out dir}
heap=${HEAP:-6g}
here=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
fixtures="$here/test/resources/fixtures"
mkdir -p "$out" "$out/base"

if [ -n "${DRIFT_SEED_LIST:-}" ]; then read -ra seeds <<< "${DRIFT_SEED_LIST//,/ }"
elif [ -n "${DRIFT_SEED:-}" ]; then seeds=("$DRIFT_SEED")
else read -ra seeds <<< "$(seq -s ' ' 1 "${DRIFT_SEEDS:-2}")"
fi
[ "${#seeds[@]}" -gt 0 ] || { echo "identity drift ($cc): no seed to replay" >&2; exit 2; }
seed_list=$(IFS=,; echo "${seeds[*]}")
seed_said=${seeds[*]}
printf '%s\n' "${seeds[@]}" > "$out/seed"

[ -f "$fixtures/corpus/cinema-scrapes-$cc.json.gz" ] || { echo "identity drift ($cc): no recorded corpus in $fixtures/corpus" >&2; exit 2; }

# <checkout> <out dir> <seeds, comma-separated> [IdentityDrift options...]: one replay, over the HEAD's recording
replay() {
  local dir=$1 to=$2 list=$3
  shift 3
  mkdir -p "$to"
  replay_code=0
  ( cd "$dir" && sbt -J-Xmx"$heap" 'set worker / Test / run / fork := false' \
      "worker/Test/runMain scripts.IdentityDrift $cc --recording $fixtures --families $fixtures/identity-unmatched/$cc.json.gz $* --seed $list --out $to" ) \
    > "$to/sbt.log" 2>&1 || replay_code=$?
}
head_options=(--allow "$fixtures/identity-drift-allow.tsv" --flips-allow "$fixtures/identity-flips-allow.tsv" --labels "$fixtures/identity-judgements.tsv")

# ---- THE STORE (see the header) ----
code_paths=(geodata/src/main base/src/main http/src/main common/src/main identity/src/main scrapers/src/main worker/src/main worker/src/it/scala
            testkit/src/main build.sbt project worker/src/test/scala/scripts)
code_of() { git -C "$here" ls-tree -r "$1" -- "${code_paths[@]}" | sha256sum | cut -c1-24; } # <sha>
recording_key=$( { printf '%s\n' "$cc" "${KINOWO_CONVERGENCE_PIN_CORPUS_RUN:-}" "${KINOWO_CONVERGENCE_TREE_ASSET:-}" "${KINOWO_CONVERGENCE_FILL_ASSETS:-}"
                   cat "$fixtures/corpus/cinema-scrapes-$cc.json.gz" "$fixtures/identity-unmatched/$cc.json.gz" "$fixtures/identity-unmatched/$cc.inputs" 2> /dev/null
                   ( cd "$fixtures" && find "enrichment-$cc" -type f -printf '%P %s\n' 2> /dev/null | LC_ALL=C sort ); } | sha256sum | cut -c1-24)
asset() { printf 'drift-%s-%s-%s-%s.tsv.gz' "$1" "$cc" "$3" "$(printf '%s\n' "$2" "$recording_key" "$3" | sha256sum | cut -c1-24)"; } # <green|calc> <code> <seed>
store=${DRIFT_STORE:-}
assets="$out/base/assets.txt"; : > "$assets"
if [ -n "$store" ]; then
  # allow-silenced: no store (or an unreadable one) is an empty one: every base is then replayed, never trusted
  timeout 120 gh release view "$store" --json assets --jq '.assets[].name' > "$assets" 2> /dev/null || : > "$assets"
fi
stored() { grep -qxF "$1" "$assets"; } # <asset>
fetch() { # <asset> <file>
  [ -n "$store" ] && stored "$1" || return 1
  timeout 300 gh release download "$store" -p "$1" -O "$2.gz" --clobber > /dev/null 2>&1 && gunzip -f "$2.gz" && [ -s "$2" ]
}
keep() { # <file> <asset>: store a result, never failing the check
  [ -n "$store" ] && [ -s "$1" ] && ! stored "$2" || return 0
  timeout 120 gh release view "$store" > /dev/null 2>&1 \
    || "$here/scripts/ci/gh-release.sh" create "$store" --prerelease --title "Identity drift store (rolling)" \
         --notes "The drift gate's flips per seed, keyed by identity code, recording and seed (scripts/ci/identity-drift-check.sh). A cache, not a version of anything." > /dev/null 2>&1 || true
  gzip -c "$1" > "${RUNNER_TEMP:-/tmp}/$2" && "$here/scripts/ci/gh-release.sh" upload "$store" "${RUNNER_TEMP:-/tmp}/$2" --clobber > /dev/null 2>&1 \
    || echo "identity drift ($cc): could not store $2 -- a later run resolves that base again"
  # only the newest are ever a base again: 48 a kind and country (best effort)
  local kind=${2#drift-}; kind=${kind%%-*}
  # allow-silenced: pruning old results is best effort; a missed prune only leaves the store larger
  timeout 120 gh release view "$store" --json assets --jq ".assets | map(select(.name | startswith(\"drift-$kind-$cc-\"))) | sort_by(.createdAt) | reverse | .[48:][] | .name" 2> /dev/null \
    | while read -r stale; do "$here/scripts/ci/gh-release.sh" delete-asset "$store" "$stale" -y > /dev/null 2>&1 || true; done
}

# ---- THE HEAD'S REPLAY, or the stored replay of the same inputs (see the header) ----
replay_inputs=(geodata/src base/src http/src common/src identity/src scrapers/src testkit/src itkit/src worker/src build.sbt identity-impact.sbt
               project .jvmopts test scripts/ci/identity-drift-check.sh scripts/ci/drift-replays-compare.sh)
head_asset=drift-head-$cc-${seed_list//,/_}-$( { printf '%s\n' "$seed_list" "$recording_key" "${JAVA_HOME:-}" "$heap"
                                                 git -C "$here" ls-tree -r HEAD -- "${replay_inputs[@]}"; } | sha256sum | cut -c1-24).tar.gz
memo=false
[ -n "$store" ] && [ "${DRIFT_TWICE:-false}" != true ] && [ -z "${DRIFT_EXPLORE:-}" ] && [ "${DRIFT_FRESH:-false}" != true ] && memo=true
if [ "$memo" = true ] && fetch "$head_asset" "$out/head-replay.tar" && tar -xf "$out/head-replay.tar" -C "$out"; then
  rm -f "$out/head-replay.tar"
  echo "identity drift ($cc): the head's replay of seed(s) $seed_said is stored under the same inputs ($head_asset): read, not replayed"
else
  rm -f "$out/head-replay.tar"
  replay "$here" "$out" "$seed_list" "${head_options[@]}"
  if [ "$memo" = true ] && [ -f "$out/drift-$cc.tsv" ] && [ -f "$out/flips-$cc.tsv" ] && grep -qE '^## [a-z]+: ' "$out/sbt.log" && grep -qE '^## [a-z]+ flips: ' "$out/sbt.log"; then
    # what the replay wrote, nothing the check adds after it
    ( cd "$out" && tar -cf "${RUNNER_TEMP:-/tmp}/head-replay-$cc.tar" sbt.log ./*-"$cc".tsv ) && keep "${RUNNER_TEMP:-/tmp}/head-replay-$cc.tar" "$head_asset"
  fi
fi
grep -E '^## |^  [0-9]+ listing|^      ' "$out/sbt.log" > "$out/report.txt"
[ -f "$out/drift-$cc.tsv" ] && [ -f "$out/flips-$cc.tsv" ] || { tail -40 "$out/sbt.log" >&2; echo "identity drift ($cc): the replay wrote nothing" >&2; exit 2; }
summary=$(grep -m1 -E '^## [a-z]+: ' "$out/report.txt")
[ -n "$summary" ] || { tail -40 "$out/sbt.log" >&2; echo "identity drift ($cc): no verdict (sbt exit $replay_code)" >&2; exit 2; }
grep -qE '^## [a-z]+ flips: ' "$out/report.txt" || { tail -40 "$out/sbt.log" >&2; echo "identity drift ($cc): no flips verdict (sbt exit $replay_code)" >&2; exit 2; }
found=$(sed -E 's/.* ([0-9]+) divergence\(s\).*/\1/' <<< "$summary")
allowed=$(sed -E 's/.* ([0-9]+) allow-listed.*/\1/' <<< "$summary")
open_divergences=$((found - allowed))
# The head's own report up to its flips (its summary, divergences and pending lines): its flip rows are not the verdict --
# the differential below is, and agentctl keys a red by the rows the log names.
sed '/^## [a-z]* flips: /,$d' "$out/report.txt"
grep -m1 -E '^## [a-z]+ flips: ' "$out/report.txt" | sed -E 's/, [0-9]+ known$//'

# ---- THE EXPLORER (DRIFT_EXPLORE set; .github/workflows/identity-drift-explorer.yml): the head (main) alone, on seeds the
# gate does not hold. Never red: its flips no backlog row names go to <out>/explore-<cc>.tsv (scripts/ci/drift_explorer.py
# new-flips), which the explorer publishes for agentctl to file -- each a backlog row and its seed onto the gate.
if [ -n "${DRIFT_EXPLORE:-}" ]; then
  python3 "$here/scripts/ci/drift_explorer.py" new-flips --backlog "$fixtures/identity-flip-backlog.tsv" --date "$(date -u +%F)"     --ref "${DRIFT_EXPLORE_REF:-}" "$out/flips-$cc.tsv" > "$out/explore-$cc.tsv"     || { echo "identity drift ($cc): the explorer could not read the head's flips" | tee "$out/verdict.txt"; exit 2; }
  fresh=$(($(wc -l < "$out/explore-$cc.tsv") - 1))
  sed 1d "$out/explore-$cc.tsv" | while IFS=$'\t' read -r v t _c s _; do echo "      explored: $v | $t [seed $s] -- not in the backlog"; done
  echo "explorer ($cc): $fresh flip(s) no backlog row names, on seed(s) $seed_said (main, head alone; never red)" | tee "$out/verdict.txt"
  [ -n "${GITHUB_STEP_SUMMARY:-}" ] && { echo "### explorer ($cc), seed(s) $seed_said"; echo; echo '```'; cat "$out/explore-$cc.tsv"; echo '```'; } >> "$GITHUB_STEP_SUMMARY"
  exit 0
fi

# ---- THE BASE (see the header) ----
depth=${DRIFT_BASE_DEPTH:-60}
head_sha=$(git -C "$here" rev-parse HEAD)
head_code=$(code_of HEAD)

# the history the walk reads (trees only), and main's tip for a branch's merge-base
# allow-silenced: a fetch that fails leaves the history the checkout has; the walk then finds fewer stored bases
timeout 300 git -C "$here" fetch -q --no-tags --filter=blob:none --depth="$depth" origin "$head_sha" 2> /dev/null || true
if [ -n "${DRIFT_BASE:-}" ]; then
  # allow-silenced: a base not fetched is said below ("no base commit"), with no verdict
  timeout 300 git -C "$here" fetch -q --no-tags --depth=1 origin "$DRIFT_BASE" 2> /dev/null || true
  default_base=$(git -C "$here" rev-parse --verify -q "${DRIFT_BASE}^{commit}") || { echo "identity drift ($cc): no base commit $DRIFT_BASE" | tee "$out/verdict.txt"; exit 2; }
else
  default_base=""
  # allow-silenced: main not fetched leaves the merge-base to the history the checkout has (the parent, said below)
  if [ "${GITHUB_REF:-}" != refs/heads/main ] && timeout 300 git -C "$here" fetch -q --no-tags --filter=blob:none --depth="$depth" origin main 2> /dev/null; then
    default_base=$(git -C "$here" merge-base HEAD FETCH_HEAD 2> /dev/null || true)
  fi
  [ -n "$default_base" ] && [ "$default_base" != "$head_sha" ] || default_base=$(git -C "$here" rev-parse --verify -q 'HEAD~1^{commit}') \
    || { echo "identity drift ($cc): could not name a base commit (no parent in the fetched history)" | tee "$out/verdict.txt"; exit 2; }
fi
ancestors=()
[ -n "${DRIFT_BASE:-}" ] || read -ra ancestors <<< "$(git -C "$here" rev-list --first-parent --max-count="$depth" HEAD~1 2> /dev/null | tr '\n' ' ')"
ancestor_codes=()
for c in "${ancestors[@]+"${ancestors[@]}"}"; do ancestor_codes+=("$(code_of "$c")"); done

recompute=()
for s in "${seeds[@]}"; do
  base=$default_base how=""
  base_code=""
  for i in "${!ancestor_codes[@]}"; do
    if stored "$(asset green "${ancestor_codes[$i]}" "$s")"; then base=${ancestors[$i]} base_code=${ancestor_codes[$i]} how=stored; break; fi
  done
  [ -n "$base_code" ] || base_code=$(code_of "$base")
  echo "$base" > "$out/base/base-$s.txt"
  if [ "$base_code" = "$head_code" ]; then how=same-code
  elif [ "$how" = stored ] && fetch "$(asset green "$base_code" "$s")" "$out/base/flips-$s.tsv"; then
    fetch "$(asset greenscenario "$base_code" "$s")" "$out/base/scenario-$s.tsv" || true
  elif fetch "$(asset calc "$base_code" "$s")" "$out/base/flips-$s.tsv"; then how=stored
    fetch "$(asset calcscenario "$base_code" "$s")" "$out/base/scenario-$s.tsv" || true
  else base=$default_base how=resolved; echo "$base" > "$out/base/base-$s.txt"
       if [ "$(code_of "$base")" = "$head_code" ]; then how=same-code; else recompute+=("$s"); fi
  fi
  echo "$how" > "$out/base/how-$s.txt"
done

if [ "${#recompute[@]}" -gt 0 ]; then
  base=$default_base
  base_dir="${RUNNER_TEMP:-$out}/drift-base-checkout"
  # A base that does not compile is a commit main merged broken (two landings green alone, red together: #950's
  # 053f867ea9, its own WhatIfAnswersSpec on the names it renamed). It judges nothing, so the base steps back to its
  # parent, at most DRIFT_BASE_STEPS (3) times, and says so; one that never compiles is still no verdict.
  steps=0
  while :; do
    echo "identity drift ($cc): resolving the base ${base:0:12} on seed(s) ${recompute[*]} (none stored)"
    # allow-silenced: a base not fetched fails its checkout below, which says so with no verdict
    timeout 300 git -C "$here" fetch -q --no-tags --depth=$((steps + 2)) origin "$base" 2> /dev/null || true
    rm -rf "$base_dir"; git -C "$here" worktree prune 2> /dev/null || true
    git -C "$here" worktree add -q --detach "$base_dir" "$base" > /dev/null 2>&1 \
      || { echo "identity drift ($cc): could not check out the base $base" | tee "$out/verdict.txt"; exit 2; }
    first=${recompute[0]}
    replay "$base_dir" "$out/base/seed-$first" "$first"
    parent=""
    if [ ! -f "$out/base/seed-$first/flips-$cc.tsv" ] && grep -q 'Compilation failed' "$out/base/seed-$first/sbt.log" \
       && [ "$steps" -lt "${DRIFT_BASE_STEPS:-3}" ]; then
      parent=$(git -C "$here" rev-parse --verify -q "${base}~1^{commit}") || parent=""
    fi
    [ -n "$parent" ] || break
    echo "::warning title=identity drift ($cc)::the base ${base:0:12} does not compile; judged against its parent ${parent:0:12} instead"
    base=$parent steps=$((steps + 1))
  done
  for s in "${recompute[@]}"; do
    echo "$base" > "$out/base/base-$s.txt"
    # one seed a replay: a base from before `--seed S1,S2` reads one seed alone
    [ "$s" = "$first" ] || replay "$base_dir" "$out/base/seed-$s" "$s"
    [ -f "$out/base/seed-$s/flips-$cc.tsv" ] \
      || { tail -40 "$out/base/seed-$s/sbt.log" >&2; echo "identity drift ($cc): the base's replay of seed $s wrote nothing, so nothing was judged" | tee "$out/verdict.txt"; exit 2; }
    cp "$out/base/seed-$s/flips-$cc.tsv" "$out/base/flips-$s.tsv"
    keep "$out/base/flips-$s.tsv" "$(asset calc "$(code_of "$base")" "$s")"
    # a base from before the scenario guard writes none
    if [ -s "$out/base/seed-$s/scenario-$cc.tsv" ]; then
      cp "$out/base/seed-$s/scenario-$cc.tsv" "$out/base/scenario-$s.tsv"
      keep "$out/base/scenario-$s.tsv" "$(asset calcscenario "$(code_of "$base")" "$s")"
    fi
  done
  git -C "$here" worktree remove --force "$base_dir" > /dev/null 2>&1 || true
fi

# ---- THE SCENARIO GUARD, per seed: head and base met the same scenario, or no flip is judged ----
: > "$out/scenario.txt"
harness_seeds=()
for s in "${seeds[@]}"; do
  how=$(cat "$out/base/how-$s.txt")
  if [ "$how" = same-code ]; then echo "scenario: the base's identity code and harness are the head's on seed $s -- the same scenario" >> "$out/scenario.txt"; continue; fi
  scenario_code=0
  python3 "$here/scripts/ci/drift_scenario.py" compare --seed "$s" --head "$out/scenario-$cc.tsv" --base "$out/base/scenario-$s.tsv" \
    >> "$out/scenario.txt" 2>&1 || scenario_code=$?
  case $scenario_code in
    0) ;;
    1) harness_seeds+=("$s") ;;
    *) cat "$out/scenario.txt"; echo "identity drift ($cc): the scenarios of seed $s could not be read" | tee "$out/verdict.txt"; exit 2 ;;
  esac
done
grep '^scenario: ' "$out/scenario.txt"
if [ "${#harness_seeds[@]}" -gt 0 ]; then
  cat "$out/scenario.txt"
  said="harness error: head and base replayed different scenarios on seed(s) ${harness_seeds[*]} -- the drift harness couples its draws to what the code asks (scripts/ci/drift_scenario.py); no flip judged"
  echo "$said" > "$out/verdict.txt"
  echo "::error title=identity drift harness ($cc, seed ${harness_seeds[0]})::$said"
  [ -z "${GITHUB_STEP_SUMMARY:-}" ] || { echo "### identity drift ($cc): HARNESS ERROR"; echo; echo '```'; cat "$out/scenario.txt"; echo '```'; } >> "$GITHUB_STEP_SUMMARY"
  echo "identity drift ($cc): $said"
  exit 2
fi

# ---- THE DIFFERENTIAL, per seed ----
: > "$out/diff.txt"
new=0 pre=0 removed=0 red_seeds=() calm_seeds=()
for s in "${seeds[@]}"; do
  base=$(cat "$out/base/base-$s.txt") how=$(cat "$out/base/how-$s.txt")
  said="base ${base:0:12} ($how)"
  if [ "$how" = same-code ]; then side=(--same-code); said="base ${base:0:12} (the same identity code: nothing replayed twice)"
  else side=(--base "$out/base/flips-$s.tsv"); fi
  diff_code=0
  python3 "$here/scripts/ci/drift_diff.py" --cc "$cc" --seed "$s" --head "$out/flips-$cc.tsv" "${side[@]}" --base-said "$said" \
    --allow "$fixtures/identity-flips-allow.tsv" --backlog "$fixtures/identity-flip-backlog.tsv" --new-out "$out/new-$s.tsv" \
    > "$out/diff-$s.txt" || diff_code=$?
  [ "$diff_code" -le 1 ] || { cat "$out/diff-$s.txt"; echo "identity drift ($cc): the differential of seed $s could not be read" | tee "$out/verdict.txt"; exit 2; }
  counts=$(tail -1 "$out/diff-$s.txt")
  sed '$d' "$out/diff-$s.txt" >> "$out/diff.txt"
  n=$(sed -E 's/.*new=([0-9]+).*/\1/' <<< "$counts"); p=$(sed -E 's/.*pre-existing=([0-9]+).*/\1/' <<< "$counts"); r=$(sed -E 's/.*removed=([0-9]+).*/\1/' <<< "$counts")
  new=$((new + n)) pre=$((pre + p)) removed=$((removed + r))
  if [ "$n" -gt 0 ]; then red_seeds+=("$s"); else calm_seeds+=("$s"); fi
done
cat "$out/diff.txt"
printf '%s\n' "${red_seeds[@]+"${red_seeds[@]}"}" "${calm_seeds[@]+"${calm_seeds[@]}"}" | grep . > "$out/seed"
if [ "$pre" -gt 0 ]; then
  echo "::notice title=identity drift ($cc)::$pre pre-existing (backlog) flip(s) on seed(s) $seed_said -- in the base too, not failed (identity-flip-backlog.tsv; the explorer files new ones)"
fi

echo "$open_divergences divergence(s) from a whole resolve and $new flip(s) on the way the change adds over its base, beyond the allow-lists; $pre pre-existing (backlog) flip(s) not failed -- seed(s) $seed_said" > "$out/verdict.txt"
judged=${red_seeds[*]:-$seed_said}
repro="IdentityDrift $cc --seed ${judged// /,}"
slice_list=$(python3 "$here/scripts/ci/drift_seeds.py" slices "$cc" "$fixtures" "$out/drift-$cc.tsv" "$out"/new-*.tsv 2> /dev/null || true)
while IFS= read -r slice; do
  [ -n "$slice" ] && repro="$repro; slice: IdentityDrift $cc --slice ${slice#"$here"/} --seed ${judged// /,}"
done <<< "$slice_list"
echo "$repro" > "$out/reproduce.txt"
if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
  { echo "### identity drift ($cc, seed(s) $seed_said): incremental vs whole, and the flips the change adds over its base"
    echo "**$(cat "$out/verdict.txt")**"; echo '```'; cat "$out/diff.txt"; echo '```'
    echo "<details><summary>the head's full report</summary>"; echo; echo '```'; cat "$out/report.txt"; echo '```'; echo '</details>'; } >> "$GITHUB_STEP_SUMMARY"
fi
if [ $((open_divergences + new)) -gt 0 ]; then
  echo "identity drift ($cc): red on seed(s) ${judged} -- reproduce: $repro"
  [ -z "${GITHUB_STEP_SUMMARY:-}" ] || echo "**red on seed(s) $judged** -- reproduce: \`$repro\`" >> "$GITHUB_STEP_SUMMARY"
fi

if [ "${DRIFT_TWICE:-false}" = true ]; then
  replay "$here" "$out/again" "$seed_list" "${head_options[@]}"
  [ -f "$out/again/drift-$cc.tsv" ] && [ -f "$out/again/flips-$cc.tsv" ] ||
    { tail -40 "$out/again/sbt.log" >&2; echo "identity drift ($cc): the second replay wrote nothing" >&2; exit 2; }
  "$here/scripts/ci/drift-replays-compare.sh" "$cc" "$out" "$out/again" "$out/verdict.txt" || exit $?
fi

# A green head on main is the base of what follows: its flips are stored per seed for the walk above.
if [ $((open_divergences + new)) -eq 0 ] && [ "${GITHUB_REF:-}" = refs/heads/main ]; then
  for s in "${seeds[@]}"; do
    awk -F'\t' -v s="$s" 'NR == 1 || $2 == s' "$out/flips-$cc.tsv" > "$out/base/head-flips-$s.tsv"
    keep "$out/base/head-flips-$s.tsv" "$(asset green "$head_code" "$s")"
    awk -F'\t' -v s="$s" 'NR == 1 || $1 == s' "$out/scenario-$cc.tsv" > "$out/base/head-scenario-$s.tsv" 2> /dev/null \
      && keep "$out/base/head-scenario-$s.tsv" "$(asset greenscenario "$head_code" "$s")"
  done
fi
[ "$open_divergences" -gt 0 ] && exit 1
[ "$new" -gt 0 ] && exit 1
exit 0

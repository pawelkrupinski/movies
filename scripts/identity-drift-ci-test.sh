#!/usr/bin/env bash
# scripts/identity-drift-ci against a scratch repository and a stub gh: --for-red reads a red run's failed takes-drift
# rows and dispatches only their countries and seeds; the verdict is the requested rows' own (a red row is DRIFT RED,
# exit 1; every row green DRIFT CLEAN, exit 0; a row that never ran NO VERDICT, exit 2); a red that failed no drift row
# has nothing to judge (exit 0, no push, no dispatch).
# Run: bash scripts/identity-drift-ci-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m identity-drift-ci\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
origin="$work/origin.git" repo="$work/repo"
scratch_repo "$origin" --bare -b main
scratch_repo "$repo" -b main
scratch_git "$repo" remote add origin "$origin"
mkdir -p "$repo/.github/workflows" "$repo/scripts/ci"
echo "drift-only: true" > "$repo/.github/workflows/country-convergence-leg.yml"
scratch_git "$repo" add -A && scratch_git "$repo" commit -qm main
scratch_git "$repo" push -q origin main
tip=$(scratch_git "$repo" rev-parse HEAD)

mkdir -p "$work/bin" "$work/state"
# gh: answers from $work/state -- runs.json (run list), jobs-<id>.json (run view --json jobs), status-<id> (run view
# --json status) -- applying --jq as gh does; `workflow run` notes its arguments and lists the dispatched run as 900;
# `run download` lays each row's artifact
cat > "$work/bin/gh" <<STUB
#!/usr/bin/env bash
state="$work/state"
jq_arg=""; prev=""
for a in "\$@"; do [ "\$prev" = --jq ] && jq_arg=\$a; prev=\$a; done
out() { if [ -n "\$jq_arg" ]; then jq -r "\$jq_arg"; else cat; fi; }
case "\$1 \$2" in
  "workflow run") echo "\$*" >> "\$state/dispatches"
    printf '[{"databaseId": 900, "number": 600, "headSha": "%s", "createdAt": "2999-01-01T00:00:00Z"}]' "$tip" > "\$state/runs-dispatched.json" ;;
  "run list")
    if [[ " \$* " == *" --event "* ]]; then out < "\$state/runs-dispatched.json"; else out < "\$state/runs.json"; fi ;;
  "run view")
    if [[ " \$* " == *" jobs"* ]]; then out < "\$state/jobs-\$3.json"
    elif [[ " \$* " == *" number"* ]]; then printf '{"number": %s}' "\$(cat "\$state/number-\$3")" | out
    else printf '{"status": "%s"}' "\$(cat "\$state/status-\$3")" | out; fi ;;
  "run download")
    dir=""; p=""; for a in "\$@"; do [ "\$p" = --dir ] && dir=\$a; p=\$a; done
    for d in "\$state"/artifacts-"\$3"/*; do [ -d "\$d" ] && cp -R "\$d" "\$dir/"; done ;;
  *) exit 1 ;;
esac
STUB
chmod +x "$work/bin/gh"

drift() { # <name> [flags...]: exit code in $work/<name>.rc, stdout in .out, stderr in .err
  local name=$1; shift
  ( cd "$repo" && KINOWO_GH="$work/bin/gh" KINOWO_IDENTITY_CACHE="$work/cache" IDENTITY_MEASURE_REMOTE=origin HOME="$work" \
      IDENTITY_DRIFT_FIRST_WAIT=0 DISPATCH_FIND_SECONDS=5 PATH="$work/bin:$PATH" \
      "$REPO_ROOT/scripts/identity-drift-ci" --head "$tip" --out "$work/out-$name" "$@" > "$work/$name.out" 2> "$work/$name.err" )
  echo $? > "$work/$name.rc"
}
jobs() { # <run id> <name:status:conclusion>...
  local id=$1 first=1; shift
  { printf '{"jobs": ['
    for j in "$@"; do IFS=: read -r n s c <<< "$j"; [ -n "$first" ] || printf ','; first=""; printf '{"name": "%s", "status": "%s", "conclusion": "%s"}' "$n" "$s" "$c"; done
    printf ']}'; } > "$work/state/jobs-$id.json"
}
artifact() { # <run id> <cc> <row> <open divergences> <open flips> [seed]: the row's artifact as identity-drift-check.sh
  # leaves it -- a `seed` file since the rotating seeds; none, a run from before them (row N replayed seed N)
  local dir="$work/state/artifacts-$1/identity-drift-$2-takes-drift-$3"
  rm -rf "$dir"; mkdir -p "$dir"
  [ -z "${6:-}" ] || { echo "$6" > "$dir/seed"; echo "IdentityDrift $2 --seed $6" > "$dir/reproduce.txt"; }
  echo "$4 divergence(s) from a whole resolve and $5 flip(s) on the way, beyond the allow-lists" > "$dir/verdict.txt"
  printf '## %s: 1 divergence(s), 0 allow-listed\n      Cinema|Film  a -> b\n' "$2" > "$dir/report.txt"
}

# the red: run id 3726 (#2517) failed takes-drift-2 (germany); its UK seed-1 row passed, a leg caller failed by cascade
echo '[{"databaseId": 3726, "number": 2517, "status": "completed"}, {"databaseId": 3700, "number": 2500, "status": "completed"}]' > "$work/state/runs.json"
jobs 3726 "takes-drift-2 (germany):completed:failure" "takes-drift-1 (united-kingdom):completed:success" "leg (germany):completed:failure"
jobs 900 "takes-drift-2 (germany):completed:failure"
echo completed > "$work/state/status-900"
artifact 3726 de 2 0 1 37
artifact 900 de 2 0 1 37
drift red --for-red 2517
check "--for-red <number>: the red's drift rows alone, on the red's OWN seed, dispatched drift-only" \
  "true" "$(grep -q -- '-f drift-only=true -f takes-countries=de -f drift-seeds=,37$' "$work/state/dispatches" && echo true || echo false)"
check "...a row no better than the red's own is a refuted fix: DRIFT RED, exit 1" \
  "1 DRIFT RED: no better than run 2517: takes-drift-2 (germany): 1 -> 1 open" "$(cat "$work/red.rc") $(tail -1 "$work/red.out")"
check "...naming the row's verdict and its report's lines" "true" \
  "$(grep -q '^takes-drift-2 (germany) \[seed 37\]: failure -- 0 divergence(s) from a whole resolve and 1 flip(s)' "$work/red.out" && grep -q 'Cinema|Film' "$work/red.out" && echo true || echo false)"
check "...and its one-line reproducer" "1" "$(grep -c '^    reproduce: IdentityDrift de --seed 37$' "$work/red.out")"
check "...pushed as a measure/ branch the run deletes" "1" "$(scratch_git "$origin" for-each-ref refs/heads/measure | wc -l | tr -d ' ')"

rm -f "$work/state/dispatches"
jobs 900 "takes-drift-2 (germany):completed:success"
drift green --for-red 3726
check "--for-red <id>: the same rows; every one green is DRIFT CLEAN, exit 0" "0 DRIFT CLEAN" "$(cat "$work/green.rc") $(tail -1 "$work/green.out")"

# A PARTIAL FIX (5df3295bd: PL's flips cleared, DE seed 2's stayed): fewer in one row, more in none, is progress
jobs 3726 "takes-drift-2 (germany):completed:failure" "takes-drift-1 (united-kingdom):completed:failure"
artifact 3726 de 2 0 2; artifact 3726 uk 1 1 3
jobs 900 "takes-drift-2 (germany):completed:failure" "takes-drift-1 (united-kingdom):completed:failure" \
  "takes-drift-1 (germany):completed:success" "takes-drift-2 (united-kingdom):completed:success"
artifact 900 de 2 0 2; artifact 900 uk 1 0 1
drift partialfix --for-red 3726
check "a partial fix is DRIFT PROGRESS, exit 0, naming what moved and what stays red" \
  "0 DRIFT PROGRESS: takes-drift-1 (united-kingdom): 4 -> 1 open -- still red: takes-drift-2 (germany) takes-drift-1 (united-kingdom)" \
  "$(cat "$work/partialfix.rc") $(tail -1 "$work/partialfix.out")"
artifact 900 de 2 1 2
drift worse --for-red 3726
check "...but one row worse refutes it, whatever else got better" "1" "$(cat "$work/worse.rc")"
rm -rf "$work/state/artifacts-3726" "$work/state/dispatches"
echo 2517 > "$work/state/number-3726"
drift nobase --for-red 3726
check "a red row with no baseline to hold it against is no verdict, exit 2" "2" "$(cat "$work/nobase.rc")"
check "...its seeds, no artifact left to name them, its rows' fixed sets (no drift-seeds pin)" "false" \
  "$(grep -q -- 'drift-seeds' "$work/state/dispatches" && echo true || echo false)"

# --against main: A CHECK'S BIRTH LANDS ONLY IF IT FINDS NOTHING MAIN'S RUN DOES NOT (commit_rules.py's rule), and an
# identity change while main is red is held to "no worse". Main's newest decided run with drift rows is 3726.
jobs 3726 "takes-drift-1 (germany):completed:success" "takes-drift-2 (germany):completed:failure" \
  "takes-drift-1 (united-kingdom):completed:success" "takes-drift-2 (united-kingdom):completed:success"
artifact 3726 de 2 0 2
jobs 900 "takes-drift-1 (germany):completed:success" "takes-drift-2 (germany):completed:failure" \
  "takes-drift-1 (united-kingdom):completed:failure" "takes-drift-2 (united-kingdom):completed:success"
artifact 900 de 2 0 2; artifact 900 uk 1 0 3
rm -f "$work/state/dispatches"
drift born --against main --countries de,uk
check "--against main: the rows replay main's run's own seeds (a run from before the rotation: 1 and 2)" "true" \
  "$(grep -q -- '-f drift-seeds=1,2$' "$work/state/dispatches" && echo true || echo false)"
check "--against main: a row red where main's was green is worse -- a born-red gate is refused, exit 1" \
  "1 DRIFT RED: worse than run 3726: takes-drift-1 (united-kingdom): 0 -> 3 open" "$(cat "$work/born.rc") $(tail -1 "$work/born.out")"
jobs 900 "takes-drift-1 (germany):completed:success" "takes-drift-2 (germany):completed:failure" \
  "takes-drift-1 (united-kingdom):completed:success" "takes-drift-2 (united-kingdom):completed:success"
drift notworse --against main --countries de,uk
check "...red only where main is, and no worse there: DRIFT NOT WORSE, exit 0" \
  "0 DRIFT NOT WORSE than run 3726: takes-drift-2 (germany): 2 -> 2 open" "$(cat "$work/notworse.rc") $(tail -1 "$work/notworse.out")"
jobs 900 "takes-drift-2 (germany):completed:failure"
drift short --against main --countries de,uk
check "...a requested row that never ran is no verdict, exit 2" "2" "$(cat "$work/short.rc")"
check "--for-red and --against together is a usage error" "2" "$(drift both --for-red 3726 --against main; cat "$work/both.rc")"

# github.com names a leg's row `leg (germany) / takes-drift-1 (germany)`: a recorded payload (run 41250027249), de and uk red
cp "$REPO_ROOT/scripts/ci/testdata/github-drift-run-41250027249-jobs.json" "$work/state/jobs-41250027249.json"
echo completed > "$work/state/status-41250027249"
echo 2600 > "$work/state/number-41250027249"
echo '[{"databaseId": 41250027249, "number": 2600, "status": "completed"}]' > "$work/state/runs.json"
drift ghrun --run 41250027249
check "--run on github.com's nested job names finds the de/uk rows (not 'nothing to judge'), DRIFT RED, exit 1" \
  "1 DRIFT RED: takes-drift-1 (germany) takes-drift-2 (united-kingdom)" "$(cat "$work/ghrun.rc") $(tail -1 "$work/ghrun.out")"
rm -f "$work/state/dispatches"
drift ghred --for-red 41250027249
check "--for-red on it names its failed rows' countries and seeds" "true" \
  "$(grep -q 'red drift rows: takes-drift-1 (germany),takes-drift-2 (united-kingdom) -> countries de,uk' "$work/ghred.err" && echo true || echo false)"

rm -f "$work/state/dispatches"
jobs 3700 "convergence (poland):completed:failure" "takes-drift-1 (poland):completed:success"
drift none --for-red 2500
check "a red with no failed drift row: nothing to judge, exit 0" "0 DRIFT CLEAN (nothing to judge)" "$(cat "$work/none.rc") $(tail -1 "$work/none.out")"
check "...and nothing dispatched" "false" "$([ -f "$work/state/dispatches" ] && echo true || echo false)"

jobs 900 "takes-drift-1 (poland):completed:success" "takes-drift-1 (spain):completed:cancelled"
drift partial --countries pl,es --seeds 1
check "--countries/--seeds are the dispatch's takes-countries and drift-seeds" "true" \
  "$(grep -q -- '-f takes-countries=pl,es -f drift-seeds=1,$' "$work/state/dispatches" && echo true || echo false)"
check "--seeds that are no seeds: a usage error, exit 2" "2" "$(drift badseeds --seeds x; cat "$work/badseeds.rc")"
check "...a row that did not pass or fail is NO VERDICT, exit 2" "2" "$(cat "$work/partial.rc")"
check "...naming it" "true" "$(tail -1 "$work/partial.out" | grep -q 'takes-drift-1 (spain) (cancelled)' && echo true || echo false)"

# --run over a run whose every drift row was skipped (its setup red): nothing judged, never DRIFT CLEAN
jobs 901 "takes-drift-1 (poland):completed:skipped" "takes-drift-2 (poland):completed:skipped"
echo completed > "$work/state/status-901"
drift allskipped --run 901
check "--run: a run whose every drift row was skipped is NO VERDICT, exit 2 -- never DRIFT CLEAN" \
  "2 NO VERDICT: run 901 has no takes-drift row that passed or failed" "$(cat "$work/allskipped.rc") $(tail -1 "$work/allskipped.out")"
jobs 901 "takes-drift-1 (poland):completed:success" "takes-drift-1 (spain):completed:skipped"
drift someskipped --run 901
check "...a dispatch's left-out countries skipped beside a green row: DRIFT CLEAN, exit 0" "0 DRIFT CLEAN" \
  "$(cat "$work/someskipped.rc") $(tail -1 "$work/someskipped.out")"

spec_summary

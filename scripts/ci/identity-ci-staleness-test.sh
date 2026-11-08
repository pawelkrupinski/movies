#!/usr/bin/env bash
# identity-ci-staleness.sh against a stub `gh` (answering `run list` per workflow from a JSON file)
# and a scratch repository whose history decides convergence coverage.
# Run: bash scripts/ci/identity-ci-staleness-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/runs"
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
[ -n "${STUB_GH_FAILS:-}" ] && exit 1
while [ $# -gt 0 ] && [ "$1" != --workflow ]; do shift; done
cat "$STUB_RUNS/$2.json" 2>/dev/null || echo '[]'
STUB
chmod +x "$work/bin/gh"

# NOW = 2026-10-14T12:00:00Z
NOW_EPOCH=1792065600
at() { echo $((NOW_EPOCH - $1 * 3600)); }                  # at <hours ago> -> epoch
iso() { date -u -r "$1" +%Y-%m-%dT%H:%M:%SZ 2>/dev/null || date -u -d "@$1" +%Y-%m-%dT%H:%M:%SZ; }

# The scratch repository: a pipeline commit 4h old (worker/src/main), then a docs-only commit on it.
repo="$work/repo"
scratch_repo "$repo" -q
commit() {  # commit <path> <hours ago> -> prints the sha
  mkdir -p "$repo/$(dirname "$1")"; echo "$RANDOM" >> "$repo/$1"
  scratch_git "$repo" add -A
  GIT_COMMITTER_DATE="@$(at "$2")" GIT_AUTHOR_DATE="@$(at "$2")" \
    scratch_git "$repo" commit -qm "$1"
  scratch_git "$repo" rev-parse HEAD
}
printf '# the paths\nworker/src/main/**\nbuild.sbt\n' > "$work/paths.txt"
base=$(commit README 10)
pipeline=$(commit worker/src/main/A.scala 4)
docs=$(commit docs/notes.md 1)

run_json() {  # run_json <sha> <status> <conclusion> <created hours ago> <completed hours ago>
  printf '{"headSha":"%s","status":"%s","conclusion":"%s","createdAt":"%s","updatedAt":"%s","url":"u/%s"}' \
    "$1" "$2" "$3" "$(iso "$(at "$4")")" "$(iso "$(at "$5")")" "$4"
}
runs() { local file="$work/runs/$1.json"; shift; (IFS=,; echo "[$*]") > "$file"; }

# A green recording 10h ago unless a case says otherwise.
green_recording() { runs record-scrape-fixtures.yml "$(run_json "$base" completed success 11 10)"; }

check_out() {  # check_out <check name> -> "stale" or "ok" for that check, from a run of the script
  (cd "$repo" && PATH="$work/bin:$PATH" NOW="$NOW_EPOCH" STUB_RUNS="$work/runs" \
     CONVERGENCE_PATHS_FILE="$work/paths.txt" bash "$REPO_ROOT/scripts/ci/identity-ci-staleness.sh") \
    | grep -E "^(::error::|::warning::|ok  )$1:" | sed -E -e 's/^::(error|warning)::.*/stale/' -e 's/^ok  .*/ok/'
}
exit_code() {
  (cd "$repo" && PATH="$work/bin:$PATH" NOW="$NOW_EPOCH" STUB_RUNS="$work/runs" STUB_GH_FAILS="${1:-}" \
     GITHUB_OUTPUT="$work/output" CONVERGENCE_PATHS_FILE="$work/paths.txt" \
     bash "$REPO_ROOT/scripts/ci/identity-ci-staleness.sh" >/dev/null 2>&1)
  echo $?
}
overdue_out() { tail -1 "$work/output"; }

green_recording

echo "convergence-red"
runs identity-model-convergence.yml "$(run_json "$docs" completed success 1 0)" "$(run_json "$pipeline" completed failure 3 2)"
check "a green newest decided run is fresh" ok "$(check_out convergence-red)"
runs identity-model-convergence.yml "$(run_json "$docs" completed failure 1 0)" \
  "$(run_json "$pipeline" completed failure 8 7)" "$(run_json "$base" completed success 9 8)"
check "red since the first failure 7h ago is stale, though the newest failure is fresh" stale "$(check_out convergence-red)"
runs identity-model-convergence.yml "$(run_json "$docs" completed failure 3 2)" "$(run_json "$base" completed success 9 8)"
check "red for 2h is not yet stale" ok "$(check_out convergence-red)"
runs identity-model-convergence.yml "$(run_json "$docs" completed cancelled 1 0)" "$(run_json "$docs" in_progress "" 0 0)" \
  "$(run_json "$pipeline" completed success 3 2)" "$(run_json "$base" completed failure 9 8)"
check "cancelled and running runs decide nothing: the newest DECIDED run is green" ok "$(check_out convergence-red)"
runs identity-model-convergence.yml "$(run_json "$docs" completed success -1 -1)" "$(run_json "$pipeline" completed failure 8 7)"
check "a run completed after NOW does not count" stale "$(check_out convergence-red)"

echo "convergence-unrun"
runs identity-model-convergence.yml "$(run_json "$base" completed success 9 8)"
check "a 4h-old pipeline commit with a run only at its parent is stale" stale "$(check_out convergence-unrun)"
runs identity-model-convergence.yml "$(run_json "$docs" completed failure 1 0)"
check "a run at a descendant covers it, whatever its verdict" ok "$(check_out convergence-unrun)"
runs identity-model-convergence.yml "$(run_json "$pipeline" queued "" 0 0)"
check "a queued run at it covers it" ok "$(check_out convergence-unrun)"
runs identity-model-convergence.yml "$(run_json "$pipeline" completed cancelled 3 3)" "$(run_json "$base" completed success 9 8)"
check "a cancelled run at it does not" stale "$(check_out convergence-unrun)"
runs identity-model-convergence.yml "$(run_json "$base" completed success 9 8)"
printf 'build.sbt\n' > "$work/paths.txt"
check "with only build.sbt a pipeline path, no commit touches it: fresh" ok "$(check_out convergence-unrun)"
printf 'worker/src/main/**\n' > "$work/paths.txt"
uncovered_young=$(commit worker/src/main/B.scala 0)
check "an uncovered pipeline commit under 3h old is not yet due a run" ok "$(check_out convergence-unrun)"
scratch_git "$repo" reset -q --hard "$docs"
[ -n "$uncovered_young" ]
mkdir -p "$work/not-a-checkout"
check "a history that cannot be read is stale, not 'no commit touches the paths'" stale \
  "$( (cd "$work/not-a-checkout" && PATH="$work/bin:$PATH" NOW="$NOW_EPOCH" STUB_RUNS="$work/runs" GIT_CEILING_DIRECTORIES="$work" \
       CONVERGENCE_PATHS_FILE="$work/paths.txt" bash "$REPO_ROOT/scripts/ci/identity-ci-staleness.sh" 2>/dev/null) \
     | grep -E '^(::error::|ok  )convergence-unrun:' | sed -e 's/^::error::.*/stale/' -e 's/^ok  .*/ok/')"

echo "takes-red and takes-unrun: Identity takes, watched as convergence is"
runs identity-model-convergence.yml "$(run_json "$docs" completed success 1 0)"
runs identity-takes.yml "$(run_json "$docs" completed failure 1 0)" \
  "$(run_json "$pipeline" completed failure 8 7)" "$(run_json "$base" completed success 9 8)"
check "a takes streak red since 7h ago is stale, though convergence is green" stale "$(check_out takes-red)"
check "...and convergence-red stays its own verdict" ok "$(check_out convergence-red)"
check "a takes run at a descendant covers the pipeline commit" ok "$(check_out takes-unrun)"
runs identity-takes.yml "$(run_json "$base" completed success 9 8)"
check "a green takes run is fresh" ok "$(check_out takes-red)"
check "a takes run only at the pipeline commit's parent leaves it unrun" stale "$(check_out takes-unrun)"
runs identity-takes.yml "$(run_json "$docs" completed success 1 0)"

echo "recording-stale"
runs identity-model-convergence.yml "$(run_json "$docs" completed success 1 0)"
check "a success 10h ago is fresh" ok "$(check_out recording-stale)"
runs record-scrape-fixtures.yml "$(run_json "$base" completed failure 2 1)" "$(run_json "$base" completed cancelled 20 19)" \
  "$(run_json "$base" completed success 41 40)"
check "a last success 40h ago is stale, whatever failed or was cancelled since" stale "$(check_out recording-stale)"
runs record-scrape-fixtures.yml "$(run_json "$base" completed failure 2 1)"
check "no success at all is stale" stale "$(check_out recording-stale)"

echo "the exit code"
green_recording
check "all fresh exits 0" 0 "$(exit_code)"
check "...with nothing overdue" overdue=false "$(overdue_out)"
runs record-scrape-fixtures.yml "$(run_json "$base" completed failure 2 1)"
check "a stale check is overdue work, not a red run: exits 0" 0 "$(exit_code)"
check "...and says overdue=true, for the report job's issue" overdue=true "$(overdue_out)"
green_recording
check "a gh that cannot list runs exits 1, never a quiet 0" 1 "$(exit_code fail)"
check "...and is overdue too" overdue=true "$(overdue_out)"

echo "a run list jq cannot read"
unreadable='{"headSha":"x","status":"completed","conclusion":"failure","createdAt":"2026-10-01T00:00:00Z","updatedAt":null,"url":"u"}'
runs identity-model-convergence.yml "$unreadable"
check "convergence runs that cannot be judged are stale, never ok" stale "$(check_out convergence-red)"
runs identity-model-convergence.yml
runs record-scrape-fixtures.yml "${unreadable/failure/success}"
check "...and so are recording runs" stale "$(check_out recording-stale)"

spec_summary

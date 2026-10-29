#!/usr/bin/env bash
# kick-convergence.sh (dispatch each convergence suite only for a push that can change its verdict): its read of the
# suite's last run works where GNU timeout(1) does not exist (stock macOS) and a hung read is bounded; an unreadable
# base, a base the checkout lacks and a diff that fails each dispatch; a push outside the paths does not.
# ConvergenceDispatchGateSpec holds the same gate from the JVM side; this runs it in seconds with the other self-tests.
# Run: bash .github/scripts/kick-convergence-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m kick-convergence.sh\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/repo"

# gh: `run list` answers $work/last (or, while $work/hang exists, records its pid there and never answers) -- the in-flight
# read (dispatch.sh's run_in_flight) $work/inflight; `workflow run` records the suite dispatched
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
case "$1 $2" in
  "run list") [[ " $* " == *" databaseId,headSha,status "* ]] && { cat "$STUB/inflight" 2>/dev/null; exit 0; }
              [ -e "$STUB/hang" ] && { echo $$ >> "$STUB/hang"; exec sleep 86400; } # never answers: blocks until killed
              echo "$*" > "$STUB/list-args"; cat "$STUB/last" 2>/dev/null ;;
  "workflow run") echo "$3" >> "$STUB/dispatched"; echo "$*" >> "$STUB/dispatch-args" ;;
esac
STUB
# timeout(1) as stock macOS has it: not at all
printf '#!/usr/bin/env bash\necho "timeout: command not found" >&2\nexit 127\n' > "$work/bin/timeout"
chmod +x "$work/bin/gh" "$work/bin/timeout"
echo "worker/src/main/**" > "$work/paths"

repo="$work/repo"
scratch_repo "$repo"
mkdir -p "$repo/worker/src/main" "$repo/docs"
echo v1 > "$repo/worker/src/main/Pipeline.scala"
scratch_git "$repo" add -A && scratch_git "$repo" commit -qm base
base=$(scratch_git "$repo" rev-parse HEAD)
echo notes > "$repo/docs/notes.md"
scratch_git "$repo" add -A && scratch_git "$repo" commit -qm docs
docs=$(scratch_git "$repo" rev-parse HEAD)
echo v2 > "$repo/worker/src/main/Pipeline.scala"
scratch_git "$repo" add -A && scratch_git "$repo" commit -qm pipeline
pipeline=$(scratch_git "$repo" rev-parse HEAD)

kick() { # <head> [matcher] -> the suites dispatched, one line; the exit status in $work/status
  rm -f "$work/dispatched"
  (cd "$repo" && spec_bounded env STUB="$work" PATH="$work/bin:$PATH" CONVERGENCE_PATHS_FILE="$work/paths" DISPATCH_RETRY_SECONDS=0 \
     KICK_CONVERGENCE_GH_TIMEOUT="${GH_BOUND:-30}" ${2:+"CHANGED_PATHS_MATCHER=$2"} \
     bash "$REPO_ROOT/.github/scripts/kick-convergence.sh" "$1" main "${SUITE:-Country convergence}" > "$work/out" 2>&1)
  echo $? > "$work/status"
  tr '\n' ' ' 2>/dev/null < "$work/dispatched" | sed 's/ $//'
}

echo "$base" > "$work/last"
check "a push outside the paths since the suite's last run is not dispatched (no timeout(1) on PATH)" "" "$(kick "$docs")"
check "...and the gate exits 0" "0" "$(cat "$work/status")"
case "$(cat "$work/list-args")" in *"--branch main"*) b=yes ;; *) b=no ;; esac
check "the base is the suite's newest run ON MAIN, never a measurement's or another branch's" "yes" "$b"
check "a pipeline change since the last run is dispatched" "Country convergence" "$(kick "$pipeline")"
# A RUN OF THIS VERY COMMIT ALREADY QUEUED OR RUNNING (run 3656 waited 54 min behind 3653, the same commit dispatched by
# the daily run 39 s earlier, then ran in full): not dispatched again
echo 3653 > "$work/inflight"
check "a commit whose run is already queued or running is not dispatched again" "" "$(kick "$pipeline")"
check "...saying which run judges it" "true" "$(grep -q 'run 3653 for .* is already queued or running' "$work/out" && echo true || echo false)"
rm -f "$work/inflight"

: > "$work/last"
check "a suite that never ran is dispatched" "Country convergence" "$(kick "$docs")"
echo 0123456789abcdef0123456789abcdef01234567 > "$work/last"
check "a base this checkout does not hold is dispatched" "Country convergence" "$(kick "$docs")"

echo "$base" > "$work/last"
# The read's bound is 30 s in every other case: at 1 s, the stub answering a loaded machine (the pre-push hook's
# eight script tests at once under /bin/bash 3.2) was cut off and the push "dispatched" (2026-10-23).
# The stub never answers, so the gate gets past it only by the read's bound (1 s here) cutting it off: its own exit and
# no gh left running prove that, never elapsed time (shell-spec.sh's HUNG STUBS).
: > "$work/hang"
check "a hung read of the last run is no base: dispatched" "Country convergence" "$(GH_BOUND=1 kick "$docs")"
check "...once the read's bound cuts it off: the gate exits 0, no gh left running" "0 0" \
  "$(cat "$work/status") $(spec_still_running "$work/hang")"
rm -f "$work/hang"

# THE REUSE HINT (scripts/ci/convergence_reuse.py find): a run it names goes out as reuse-run, for the run's preflight to
# check; no run, or an answer that is no run id, is the plain dispatch.
printf '#!/usr/bin/env bash\necho "${FOUND:-}"\n' > "$work/finder"; chmod +x "$work/finder"
echo "$base" > "$work/last"
rm -f "$work/dispatch-args"
reuse_args() { # <finder's answer> [suite] -> the dispatch's arguments
  rm -f "$work/dispatch-args"
  FOUND=$1 SUITE=${2:-Identity model convergence} CONVERGENCE_REUSE_FINDER="$work/finder" kick "$pipeline" > /dev/null
  cat "$work/dispatch-args"
}
check "a run the finder names goes out as reuse-run" "workflow run Identity model convergence --ref main -f tier=gate-lane -f reuse-run=2821" \
  "$(reuse_args 2821)"
check "no run found: the plain dispatch" "workflow run Identity model convergence --ref main -f tier=gate-lane" "$(reuse_args "")"
check "an answer that is no run id: the plain dispatch" "workflow run Identity model convergence --ref main -f tier=gate-lane" \
  "$(reuse_args "2821 -f takes-only=true")"
check "another suite is never handed a reuse-run" "workflow run Country convergence --ref main -f tier=gate-lane" \
  "$(reuse_args 2821 "Country convergence")"
printf '#!/usr/bin/env bash\nexit 1\n' > "$work/broken-matcher"; chmod +x "$work/broken-matcher"
check "a diff that cannot be matched is not 'nothing changed': dispatched" "Country convergence" "$(kick "$docs" "$work/broken-matcher")"

spec_summary

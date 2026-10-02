#!/usr/bin/env bash
# wait-for-run-artifact.sh against a stub `gh` whose answers a state file scripts call by call.
# Run: bash scripts/ci/wait-for-run-artifact-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m wait-for-run-artifact.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
export GITHUB_REPOSITORY=owner/repo GITHUB_RUN_ID=42 POLL_SECONDS=0 PATH="$work:$PATH"

# The stub answers the artifacts listing with the names in $work/artifacts-<n> (n = how many
# artifact listings came before, the last file repeating) and the jobs listing with $work/jobs.
cat > "$work/gh" <<'STUB'
#!/usr/bin/env bash
dir="$(dirname "$0")"
case "$2" in
  *artifacts*)
    n=$(cat "$dir/calls" 2>/dev/null || echo 0); echo $((n + 1)) > "$dir/calls"
    f="$dir/artifacts-$n"; [ -f "$f" ] || f=$(ls "$dir"/artifacts-* | sort -t- -k2 -n | tail -1)
    cat "$f" ;;
  *jobs*) cat "$dir/jobs" ;;
esac
STUB
chmod +x "$work/gh"

scenario() { rm -f "$work"/artifacts-* "$work/calls"; : > "$work/jobs"; }
run() { bash "$REPO_ROOT/scripts/ci/wait-for-run-artifact.sh" stage-worker "e2e (staging)" "$1" >/dev/null 2>&1; echo $?; }

scenario; printf 'stage-worker\n' > "$work/artifacts-0"
check "ready at once when the artifact is already listed" 0 "$(run 60)"

scenario; : > "$work/artifacts-0"; : > "$work/artifacts-1"; printf 'stage-web\nstage-worker\n' > "$work/artifacts-2"
check "waits through listings without it until it appears" 0 "$(run 60)"
check "…having asked three times" 3 "$(cat "$work/calls")"

scenario; printf 'stage-web\n' > "$work/artifacts-0"; printf 'failure\n' > "$work/jobs"
check "gives up the moment the producer finished without it" 1 "$(run 60)"

scenario; : > "$work/artifacts-0"; printf 'stage-worker\n' > "$work/artifacts-1"; printf 'success\n' > "$work/jobs"
check "takes an upload that landed as the producer finished" 0 "$(run 60)"

scenario; : > "$work/artifacts-0"
check "times out while the producer is still running" 1 "$(run 0)"

spec_summary

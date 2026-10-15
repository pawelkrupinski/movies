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

# gh: `run list` answers $work/last (or hangs while $work/hang exists); `workflow run` records the suite dispatched
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
case "$1 $2" in
  "run list") [ -e "$STUB/hang" ] && sleep 30; cat "$STUB/last" 2>/dev/null ;;
  "workflow run") echo "$3" >> "$STUB/dispatched" ;;
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
  (cd "$repo" && env STUB="$work" PATH="$work/bin:$PATH" CONVERGENCE_PATHS_FILE="$work/paths" DISPATCH_RETRY_SECONDS=0 \
     KICK_CONVERGENCE_GH_TIMEOUT=1 ${2:+"CHANGED_PATHS_MATCHER=$2"} \
     bash "$REPO_ROOT/.github/scripts/kick-convergence.sh" "$1" main "Country convergence" > "$work/out" 2>&1)
  echo $? > "$work/status"
  tr '\n' ' ' < "$work/dispatched" 2>/dev/null | sed 's/ $//'
}

echo "$base" > "$work/last"
check "a push outside the paths since the suite's last run is not dispatched (no timeout(1) on PATH)" "" "$(kick "$docs")"
check "...and the gate exits 0" "0" "$(cat "$work/status")"
check "a pipeline change since the last run is dispatched" "Country convergence" "$(kick "$pipeline")"

: > "$work/last"
check "a suite that never ran is dispatched" "Country convergence" "$(kick "$docs")"
echo 0123456789abcdef0123456789abcdef01234567 > "$work/last"
check "a base this checkout does not hold is dispatched" "Country convergence" "$(kick "$docs")"

echo "$base" > "$work/last"
touch "$work/hang"
start=$SECONDS
check "a hung read of the last run is no base: dispatched" "Country convergence" "$(kick "$docs")"
check "...once the read's bound (1 s here) ends it, not gh's 30 s" "yes" "$([ $((SECONDS - start)) -lt 15 ] && echo yes || echo no)"
rm -f "$work/hang"

printf '#!/usr/bin/env bash\nexit 1\n' > "$work/broken-matcher"; chmod +x "$work/broken-matcher"
check "a diff that cannot be matched is not 'nothing changed': dispatched" "Country convergence" "$(kick "$docs" "$work/broken-matcher")"

spec_summary

#!/usr/bin/env bash
# convergence-bisect-step.sh in a scratch repository, against a stub `sbt` and `timeout`.
# Run: bash .github/scripts/convergence-bisect-step-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m convergence-bisect-step.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
step="$REPO_ROOT/.github/scripts/convergence-bisect-step.sh"
mkdir -p "$work/bin" "$work/tmp"

# The stub sbt answers as STUB_SBT says, and names what it was asked.
cat > "$work/bin/sbt" <<'STUB'
#!/usr/bin/env bash
echo "sbt $*"
case "${STUB_SBT:-green}" in
  green) exit 0 ;; red) exit 1 ;; unrecorded) echo "a HERMETIC run needed a response"; exit 1 ;;
esac
STUB
# timeout <seconds> <cmd...>: STUB_TIMEOUT=expire answers as an expired one does (124).
cat > "$work/bin/timeout" <<'STUB'
#!/usr/bin/env bash
echo "$1" > "$(dirname "$0")/../timeout-seconds"
shift
[ "${STUB_TIMEOUT:-}" = expire ] && exit 124
"$@"
STUB
chmod +x "$work/bin/sbt" "$work/bin/timeout"

scratch_repo "$work/repo"
cd "$work/repo" || exit 1
mkdir -p test/resources worker/src/fixtures/scala/tools
echo committed > test/resources/corpus.txt
echo 'object ArchiveReplayWiring' > worker/src/fixtures/scala/tools/ArchiveReplayWiring.scala
scratch_git . add -A && scratch_git . commit -qm "before hermetic replay"
old=$(scratch_git . rev-parse HEAD)
echo 'val HermeticVar = "KINOWO_CONVERGENCE_HERMETIC"' >> worker/src/fixtures/scala/tools/ArchiveReplayWiring.scala
scratch_git . commit -qam "hermetic replay"
# The pinned corpus, which overwrites the committed one for the step.
mkdir -p "$work/corpus/test/resources" scrape-archive
echo pinned > "$work/corpus/test/resources/corpus.txt"
tar -czf scrape-archive/scrapes-pl.tar.gz -C "$work/corpus" test

run() { PATH="$work/bin:$PATH" RUNNER_TEMP="$work/tmp" CODE=pl STEP_MINUTES=2 HEAP=6g SAMPLE_COMMAND="worker/It/testOnly Sample" "$@" bash "$step" > "$work/out" 2>&1; echo $?; }

check "a green sample is good" "0" "$(run env STUB_SBT=green)"
check "it runs the sample with the heap asked for" "sbt -J-Xmx6g worker/It/testOnly Sample" "$(cat "$work/tmp/bisect-step.log")"
check "within the step's minutes" "120" "$(cat "$work/timeout-seconds")"
check "it names the commit and the sample's exit" "$(scratch_git . rev-parse --short HEAD): sample exit 0" "$(cat "$work/out")"
check "and puts the committed fixtures back" "committed" "$(cat test/resources/corpus.txt)"
check "a red sample is bad" "1" "$(run env STUB_SBT=red)"
check "a request the recording never saw is untestable" "125" "$(run env STUB_SBT=unrecorded)"
check "a timed-out sample is untestable" "125" "$(run env STUB_TIMEOUT=expire)"
scratch_git . checkout -q "$old"
check "a commit older than hermetic replay is untestable" "125" "$(run env STUB_SBT=green)"

spec_summary

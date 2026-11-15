#!/usr/bin/env bash
# scripts/ci/drift-explorer-publish.sh end to end against a stub `gh`: the rows' explore-<cc>.tsv, downloaded into a
# directory an artifact each, are merged and published whatever the rows ended (Explorer #1, run 41987059524, published
# nothing when its run failed); a directory with no explore-<cc>.tsv fails it; the dated copies beyond the newest 30 go.
# The script dispatches nothing: the rows run in the explorer's own run (Identity takes, called), never as a run of a
# gate workflow.
# Run: bash scripts/ci/drift-explorer-publish-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m drift-explorer-publish.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"
# Every call logged to $WORK/calls. `release view --json` answers $ASSETS through the caller's --jq; `release view <tag>`
# fails unless $RELEASE_EXISTS. Anything else -- a dispatch, a run list -- is unexpected.
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
echo "$*" >> "$WORK/calls"
jqexpr="" json=""
for ((i = 1; i <= $#; i++)); do
  case "${!i}" in --jq) j=$((i + 1)); jqexpr="${!j}" ;; --json) json=1 ;; esac
done
case "$1 $2" in
  "release view") if [ -n "$json" ]; then jq -r "$jqexpr" <<< "$ASSETS"; else [ -n "${RELEASE_EXISTS:-}" ]; fi ;;
  "release "*) exit 0 ;;
  *) echo "stub gh: unexpected $*" >&2; exit 1 ;;
esac
STUB
chmod +x "$work/bin/gh"
export PATH="$work/bin:$PATH" WORK="$work" GITHUB_RUN_ID=77 GH_REPO=o/r GH_RELEASE_RETRY_DELAY=0
header='venue\trawTitle\tcountry\tseed\tfirstSeen\tslice\ttakes\n'
flip='Kino Stub Testowe\tNo Such Film Anywhere\tpl\t7\t2026-11-05\tidentity-drift-pl-takes-drift-1\tA -> B\n'
publish() { # -> exit code; stdout+stderr in $WORK/out, the step summary in $WORK/summary
  rm -rf "$work/calls" "$work/temp" "$work/artifacts"; mkdir -p "$work/temp" "$work/artifacts"; : > "$work/calls"; : > "$work/summary"
  if [ -n "${EXPLORE:-}" ]; then
    mkdir -p "$work/artifacts/identity-drift-pl-takes-drift-1"
    printf '%b' "$EXPLORE" > "$work/artifacts/identity-drift-pl-takes-drift-1/explore-pl.tsv"
  fi
  RUNNER_TEMP="$work/temp" GITHUB_STEP_SUMMARY="$work/summary" bash "$REPO_ROOT/scripts/ci/drift-explorer-publish.sh" "$work/artifacts" > "$work/out" 2>&1
  echo $?; }
uploads() { grep -c '^release upload identity-flip-explorer .*/new-flips.tsv .*/new-flips-[0-9-]*-77.tsv --clobber$' "$work/calls"; }

export ASSETS='{"assets":[]}' EXPLORE="$header$flip"
check "the rows' flips are published" "0 1 1" \
  "$(publish) $(uploads) $(grep -c 'run 77: 1 flip(s) no backlog row names, from 1 row(s)' "$work/out")"
check "...into a release it creates when there is none" "1" "$(grep -c '^release create identity-flip-explorer --prerelease' "$work/calls")"
check "...the flip in the step summary" "1" "$(grep -c 'Kino Stub Testowe' "$work/summary")"
check "...and no workflow dispatched: a run of a gate workflow is never the explorer's" "0" \
  "$(grep -c '^workflow run\|^run list' "$work/calls")"
check "into the release already there, the same way" "0 1 0" \
  "$(RELEASE_EXISTS=1 publish) $(uploads) $(grep -c "^release create" "$work/calls")"
check "rows with no explore-<cc>.tsv fail, said, and publish nothing" "1 1 0" \
  "$(EXPLORE='' publish) $(grep -c 'wrote no explore-<cc>.tsv' "$work/out") $(uploads)"
check "no directory named is a usage error" "1" \
  "$(RUNNER_TEMP="$work/temp" GITHUB_STEP_SUMMARY="$work/summary" bash "$REPO_ROOT/scripts/ci/drift-explorer-publish.sh" > /dev/null 2>&1; echo $?)"
asset() { printf '{"name":"new-flips-2026-10-%02d-%s.tsv","createdAt":"2026-10-%02dT00:41:00Z"}' "$1" "$1" "$1"; }
ASSETS="{\"assets\":[{\"name\":\"new-flips.tsv\",\"createdAt\":\"2026-11-05T00:00:00Z\"}$(for d in $(seq 1 32); do printf ',%s' "$(asset "$d")"; done)]}"
check "the dated copies beyond the newest 30 go, the oldest first" "0 2 1 1" \
  "$(RELEASE_EXISTS=1 publish) $(grep -c '^release delete-asset' "$work/calls") $(grep -c '^release delete-asset identity-flip-explorer new-flips-2026-10-01-1.tsv -y$' "$work/calls") $(grep -c '^release delete-asset identity-flip-explorer new-flips-2026-10-02-2.tsv -y$' "$work/calls")"

spec_summary

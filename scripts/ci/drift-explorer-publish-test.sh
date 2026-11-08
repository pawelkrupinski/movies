#!/usr/bin/env bash
# scripts/ci/drift-explorer-publish.sh end to end against a stub `gh`: the dispatched drift-only run ends `failure` by
# design (its `verdict complete`, G7), and the explorer still publishes its rows' flips -- Explorer #1 (run 41987059524)
# ended at that failure, under `set -e`, and published nothing. A run with no explore-<cc>.tsv fails it; the dated
# copies beyond the newest 30 go.
# Run: bash scripts/ci/drift-explorer-publish-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m drift-explorer-publish.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"
# Every call logged to $WORK/calls. `run list` and `release view --json` answer $RUNS / $ASSETS through the caller's
# --jq; `run view` (ci-wait) answers $VIEW; `run download` writes $EXPLORE (when set) as a row's explore-pl.tsv;
# `release view <tag>` fails unless $RELEASE_EXISTS.
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
echo "$*" >> "$WORK/calls"
jqexpr="" dir="" json=""
for ((i = 1; i <= $#; i++)); do
  case "${!i}" in --jq) j=$((i + 1)); jqexpr="${!j}" ;; --dir) j=$((i + 1)); dir="${!j}" ;; --json) json=1 ;; esac
done
case "$1 $2" in
  "workflow run") exit 0 ;;
  "api repos/o/r/commits/main") echo "${MAIN_SHA:-}" ;;
  "run list") jq -r "$jqexpr" <<< "$RUNS" ;;
  "run view") echo "$VIEW" ;;
  "run download")
    [ -n "${EXPLORE:-}" ] || exit 0
    mkdir -p "$dir/identity-drift-pl-takes-drift-1"; printf '%b' "$EXPLORE" > "$dir/identity-drift-pl-takes-drift-1/explore-pl.tsv" ;;
  "release view") if [ -n "$json" ]; then jq -r "$jqexpr" <<< "$ASSETS"; else [ -n "${RELEASE_EXISTS:-}" ]; fi ;;
  "release "*) exit 0 ;;
  *) echo "stub gh: unexpected $*" >&2; exit 1 ;;
esac
STUB
chmod +x "$work/bin/gh"
sha=$(printf 'a%.0s' $(seq 40))
export PATH="$work/bin:$PATH" WORK="$work" GITHUB_SHA="$sha" GITHUB_SERVER_URL=https://github.com GH_REPO=o/r \
  DISPATCH_POLL_SECONDS=0 GH_RELEASE_RETRY_DELAY=0
export RUNS="[{\"databaseId\":77,\"headSha\":\"$sha\",\"createdAt\":\"2099-01-01T00:00:00Z\",\"displayTitle\":\"t\"}]"
header='venue\trawTitle\tcountry\tseed\tfirstSeen\tslice\ttakes\n'
flip='Kino Stub Testowe\tNo Such Film Anywhere\tpl\t7\t2026-11-05\tidentity-drift-pl-takes-drift-1\tA -> B\n'
view() { printf '{"databaseId":77,"headSha":"%s","headBranch":"main","workflowName":"w","status":"completed","conclusion":"%s"}' "$sha" "$1"; }
publish() { # -> exit code; stdout+stderr in $WORK/out, the step summary in $WORK/summary
  rm -rf "$work/calls" "$work/temp"; mkdir -p "$work/temp"; : > "$work/calls"; : > "$work/summary"
  RUNNER_TEMP="$work/temp" GITHUB_STEP_SUMMARY="$work/summary" bash "$REPO_ROOT/scripts/ci/drift-explorer-publish.sh" > "$work/out" 2>&1
  echo $?; }
uploads() { grep -c '^release upload identity-flip-explorer .*/new-flips.tsv .*/new-flips-[0-9-]*-77.tsv --clobber$' "$work/calls"; }

export VIEW ASSETS='{"assets":[]}' EXPLORE="$header$flip"
VIEW=$(view failure)
check "a drift-only run ending failure (its verdict complete, by design) still publishes its rows' flips" "0 1 1" \
  "$(publish) $(uploads) $(grep -c 'run 77: 1 flip(s) no backlog row names, from 1 row(s)' "$work/out")"
check "...into a release it creates when there is none" "1" "$(grep -c '^release create identity-flip-explorer --prerelease' "$work/calls")"
check "...the flip in the step summary" "1" "$(grep -c 'Kino Stub Testowe' "$work/summary")"
check "...dispatched drift-only, exploring, on main" "1" \
  "$(grep -c '^workflow run identity-takes.yml --ref main -f drift-only=true -f drift-explore=auto$' "$work/calls")"
VIEW=$(view success)
check "a green run publishes the same way, into the release already there" "0 1 0" \
  "$(RELEASE_EXISTS=1 publish) $(uploads) $(grep -c '^release create' "$work/calls")"
VIEW=$(view failure)
check "a run with no explore-<cc>.tsv fails, said, and publishes nothing" "1 1 0" \
  "$(EXPLORE='' publish) $(grep -c 'wrote no explore-<cc>.tsv' "$work/out") $(uploads)"
# Explorer #5 (run 42455836290): the explorer's job ran on 4d8307317e, main had moved to c903f3384d by the dispatch, and
# `--ref main` ran the drift rows there -- a run matched by GITHUB_SHA alone was "no run appeared in 120s".
check "the run is found when main moved past the explorer's commit before the dispatch" "0 1" \
  "$(MAIN_SHA=bbbb DISPATCH_FIND_SECONDS=0 RUNS='[{"databaseId":77,"headSha":"bbbb","createdAt":"2099-01-01T00:00:00Z","displayTitle":"t"}]' RELEASE_EXISTS=1 publish) $(uploads)"
check "...and a run on some other commit is still never taken for it" "1 1" \
  "$(MAIN_SHA=bbbb DISPATCH_FIND_SECONDS=0 RUNS='[{"databaseId":77,"headSha":"cccc","createdAt":"2099-01-01T00:00:00Z","displayTitle":"t"}]' RELEASE_EXISTS=1 publish) $(grep -c 'no run of it appeared' "$work/out")"
asset() { printf '{"name":"new-flips-2026-10-%02d-%s.tsv","createdAt":"2026-10-%02dT00:41:00Z"}' "$1" "$1" "$1"; }
ASSETS="{\"assets\":[{\"name\":\"new-flips.tsv\",\"createdAt\":\"2026-11-05T00:00:00Z\"}$(for d in $(seq 1 32); do printf ',%s' "$(asset "$d")"; done)]}"
check "the dated copies beyond the newest 30 go, the oldest first" "0 2 1 1" \
  "$(RELEASE_EXISTS=1 publish) $(grep -c '^release delete-asset' "$work/calls") $(grep -c '^release delete-asset identity-flip-explorer new-flips-2026-10-01-1.tsv -y$' "$work/calls") $(grep -c '^release delete-asset identity-flip-explorer new-flips-2026-10-02-2.tsv -y$' "$work/calls")"

spec_summary

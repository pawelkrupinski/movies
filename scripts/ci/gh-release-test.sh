#!/usr/bin/env bash
# gh-release.sh and publish-rolling-release.sh against a stub `gh` that records its calls and
# fails the first STUB_FAILS of them with STUB_ERR on stderr.
# Run: bash scripts/ci/gh-release-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m gh-release.sh / publish-rolling-release.sh\n'

stub_dir="$(mktemp -d)"
trap 'rm -rf "$stub_dir"' EXIT
cat > "$stub_dir/gh" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG"
n=$(($(cat "$STUB_COUNT") + 1)); echo "$n" > "$STUB_COUNT"
if [ "$n" -le "${STUB_FAILS:-0}" ]; then echo "$STUB_ERR" >&2; exit 1; fi
# `view` of a release the stub was told is missing answers the way gh does: no HTTP code.
if [ "$2" = "view" ] && [ -n "${STUB_MISSING:-}" ]; then echo "release not found" >&2; exit 1; fi
if [ "$1" = "api" ] && [ -n "${STUB_FAIL_API:-}" ]; then echo "HTTP 403: Resource not accessible by integration" >&2; exit 1; fi
echo "ok: $*"
STUB
chmod +x "$stub_dir/gh"

export GH_RELEASE_RETRY_DELAY=0 GITHUB_SHA=abc1234def GITHUB_REPOSITORY=o/r
export STUB_LOG="$stub_dir/log" STUB_COUNT="$stub_dir/count"
reset() { : > "$STUB_LOG"; echo 0 > "$STUB_COUNT"; }

# release <fails> <stderr> <args...>  -> prints "<exit status> <calls made>"
release() {
  reset; export STUB_FAILS="$1" STUB_ERR="$2"; shift 2
  PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/gh-release.sh" "$@" >/dev/null 2>&1
  echo "$? $(wc -l < "$STUB_LOG" | tr -d ' ')"
}

# Every `github-release` row of the shared retry-classification table, fed through the script
# as the stderr gh prints for it: a transient one is retried once and succeeds (2 calls), a
# permanent one fails on the first call.
table="$REPO_ROOT/test/resources/retry-classification.json"
stderr_for() {
  case "$1" in
    resource-not-accessible-by-integration)
      echo 'HTTP 403: Resource not accessible by integration (https://api.github.com/repos/o/r/releases/1)' ;;
    secondary-rate-limit) echo 'HTTP 403: You have exceeded a secondary rate limit. Please wait a few minutes before you try again.' ;;
    release-not-found)    echo 'release not found' ;;
    http:*)               echo "HTTP ${1#http:}: Something (https://api.github.com/repos/o/r/releases/1)" ;;
    *)                    echo "no sample stderr for '$1'" >&2; return 1 ;;
  esac
}
rows=0
while IFS=$'\t' read -r error verdict; do
  rows=$((rows + 1))
  sample="$(stderr_for "$error")" || { check "table row $error has a sample stderr" yes no; continue; }
  case "$verdict" in
    transient) expected="0 2" ;;
    *)         expected="1 1" ;;
  esac
  check "github-release/$error is $verdict" "$expected" "$(release 1 "$sample" edit t)"
done < <(jq -r '.rows[] | select(.source == "github-release") | [.error, .verdict] | @tsv' "$table")
check "the table has github-release rows to hold the script to" yes "$([ "$rows" -gt 0 ] && echo yes || echo no)"

check "a transient failure that never clears gives up after four attempts, failing" "1 4" \
  "$(release 99 'HTTP 502: Bad Gateway' edit t)"
check "the refused 403 says it is a permission verdict and names the grant" "1" \
  "$(reset; STUB_FAILS=1 STUB_ERR='HTTP 403: Resource not accessible by integration' PATH="$stub_dir:$PATH" \
      bash "$REPO_ROOT/scripts/ci/gh-release.sh" edit t 2>&1 >/dev/null | grep -c '::error::.*not retried.*contents: write')"
check "the arguments reach gh unchanged, under 'release'" "release edit t --notes a b" \
  "$(reset; STUB_FAILS=0 PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/gh-release.sh" edit t --notes "a b" >/dev/null; cat "$STUB_LOG")"
check "gh's stdout passes through" "ok: release view t" \
  "$(reset; STUB_FAILS=0 PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/gh-release.sh" view t)"
check "gh's stderr passes through on a final failure" "HTTP 404: Not Found" \
  "$(reset; STUB_FAILS=1 STUB_ERR='HTTP 404: Not Found' PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/gh-release.sh" view t 2>&1 >/dev/null)"

# publish <fails> <stderr> <missing?>  -> prints the subcommands gh was called with
publish() {
  reset; export STUB_FAILS="$1" STUB_ERR="$2" STUB_MISSING="$3"
  PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/publish-rolling-release.sh" \
    rolling "Rolling (latest)" "notes" "$stub_dir/a.apk" "$stub_dir/a.aab" >/dev/null 2>&1
  echo "$? $(awk '{ printf "%s ", ($1 == "release") ? $2 : $1 }' "$STUB_LOG")"
}

check "an existing rolling release is edited, never recreated, its assets clobbered, then its tag moved" \
  "0 view edit upload api " "$(publish 0 '' '')"
check "a missing one is created" "0 view create upload " "$(publish 0 '' yes)"
check "a transient failure of the existence check is retried, not mistaken for a missing release" \
  "0 view view edit upload api " "$(publish 1 'HTTP 502: Bad Gateway' '')"
check "a refused 403 on the existence check fails, never answered with a create" \
  "1 view " "$(publish 1 'HTTP 403: Resource not accessible by integration' '')"
check "the edit retargets the release at this commit, as a prerelease" \
  "release edit rolling --repo o/r --target abc1234def --prerelease --notes notes" \
  "$(publish 0 '' '' >/dev/null; grep '^release edit' "$STUB_LOG")"
check "an existing release's tag is force-moved to this commit, since --target moves only a new tag" \
  "api -X PATCH repos/o/r/git/refs/tags/rolling -f sha=abc1234def -F force=true" \
  "$(publish 0 '' '' >/dev/null; grep '^api' "$STUB_LOG")"
check "a refused tag move (assets already out) fails the publish rather than leaving the tag silently stale" \
  "1 view edit upload api " "$(STUB_FAIL_API=yes publish 0 '' '')"
check "the upload clobbers every asset given" \
  "release upload rolling --repo o/r --clobber $stub_dir/a.apk $stub_dir/a.aab" \
  "$(publish 0 '' '' >/dev/null; grep '^release upload' "$STUB_LOG")"

spec_summary

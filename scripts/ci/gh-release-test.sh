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

check "a transient 403 is retried and then succeeds" "0 2" \
  "$(release 1 'HTTP 403: Resource not accessible by integration (https://api.github.com/repos/o/r/releases/1)' edit t)"
check "a 409 conflict is retried" "0 3" "$(release 2 'HTTP 409: Conflict' upload t f)"
check "a 5xx is retried" "0 2" "$(release 1 'HTTP 502: Bad Gateway' create t)"
check "a 403 that never clears gives up after four attempts, failing" "1 4" \
  "$(release 99 'HTTP 403: Resource not accessible by integration' edit t)"
check "a 404 is NOT retried — it is an answer, not a race" "1 1" "$(release 1 'HTTP 404: Not Found' view t)"
check "a 422 validation error is NOT retried" "1 1" "$(release 1 'HTTP 422: Validation Failed' create t)"
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
  echo "$? $(cut -d' ' -f2 "$STUB_LOG" | tr '\n' ' ')"
}

check "an existing rolling release is edited, never recreated, then its assets clobbered" \
  "0 view edit upload " "$(publish 0 '' '')"
check "a missing one is created" "0 view create upload " "$(publish 0 '' yes)"
check "a 403 on the existence check is retried, not mistaken for a missing release" \
  "0 view view edit upload " "$(publish 1 'HTTP 403: Resource not accessible by integration' '')"
check "the edit retargets the release at this commit, as a prerelease" \
  "release edit rolling --repo o/r --target abc1234def --prerelease --notes notes" \
  "$(publish 0 '' '' >/dev/null; grep '^release edit' "$STUB_LOG")"
check "the upload clobbers every asset given" \
  "release upload rolling --repo o/r --clobber $stub_dir/a.apk $stub_dir/a.aab" \
  "$(publish 0 '' '' >/dev/null; grep '^release upload' "$STUB_LOG")"

spec_summary

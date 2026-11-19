#!/usr/bin/env bash
# sbt-deps-resolve.sh: resolves through the sbt guard, which retries a failed download after each backoff, and leaves
# no coursier negative record in the cache ci.yml's `deps` job then saves.
# Run: bash scripts/ci/sbt-deps-resolve-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m sbt-deps-resolve.sh\n'
script="$REPO_ROOT/scripts/ci/sbt-deps-resolve.sh"
dir="$(mktemp -d)"; trap 'rm -rf "$dir"' EXIT
mkdir -p "$dir/bin"
# A fake sbt that fails as lm-coursier does on a 429 until its FAILS budget is spent, logging each call's arguments,
# and, like coursier on a 404 for a jar whose pom it holds, leaves a negative record behind.
cat > "$dir/bin/sbt" <<'FAKE'
#!/usr/bin/env bash
echo "$*" >> "$CALLS"
mkdir -p "$COURSIER_CACHE/v1/x"; : > "$COURSIER_CACHE/v1/x/a-1.0.pom"; : > "$COURSIER_CACHE/v1/x/.a-1.0-sources.jar.error"
n=$(wc -l < "$CALLS")
[ "$n" -gt "$FAILS" ] && exit 0
echo "[error] sbt.librarymanagement.ResolveException: Error downloading a:a:1.0"
echo "[error]   download error: Caught java.io.IOException (Server returned HTTP response code: 429 for URL: https://repo1.maven.org/a.pom)"
exit 1
FAKE
chmod +x "$dir/bin/sbt"
# CI's PATH: the guard first (setup-sbt-build), the real sbt behind it.
export PATH="$REPO_ROOT/scripts/ci/sbt-guard:$dir/bin:$PATH" CALLS="$dir/calls" HOME="$dir/home" \
  COURSIER_CACHE="$dir/coursier" SBT_GUARD_DOWNLOAD_BACKOFF="0 0 0"

run() { rm -rf "$CALLS" "$COURSIER_CACHE"; FAILS="$1" "$script" > "$dir/out" 2>&1; echo $?; }

check "resolves on the first attempt" "0" "$(run 0)"
check "...running sbt update once" "1 -Dsbt.supershell=false -Dsbt.color=false update" \
  "$(wc -l < "$CALLS" | tr -d ' ') $(cat "$CALLS")"
check "...leaving no coursier negative record to save, the cache kept" "a-1.0.pom" \
  "$(find "$COURSIER_CACHE/v1/x" -mindepth 1 -exec basename {} \; | sort | paste -sd' ' -)"
check "a 429 on two attempts is retried to success" "0" "$(run 2)"
check "...in three attempts" "3" "$(wc -l < "$CALLS" | tr -d ' ')"
check "every attempt failing fails the step" "1" "$(run 9)"
check "...after four attempts, one more than the guard's three backoffs" "4" "$(wc -l < "$CALLS" | tr -d ' ')"
check "...naming them" "1" "$(grep -c '::error::sbt-deps: the build.s dependencies did not resolve' "$dir/out")"

spec_summary

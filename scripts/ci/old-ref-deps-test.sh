#!/usr/bin/env bash
# The old refs' dependency cache key (old-ref-deps-key.sh) and the offline environment a job restored from it runs
# under (jvm-offline-env.sh): the key follows the floors alone, and a JVM under that environment reaches no network.
# Run: bash scripts/ci/old-ref-deps-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m old-ref-deps\n'
work="$(cd "$(mktemp -d)" && pwd -P)"
trap 'rm -rf "$work"' EXIT

out=$(RUNNER_OS=Linux bash "$REPO_ROOT/scripts/ci/old-ref-deps-key.sh"); code=$?
check "the key script succeeds" "0" "$code"
floors=$(bash "$REPO_ROOT/scripts/identity-slice-old-ref-test.sh" --floors)
check "the old-ref test lists its floors, one a line, sorted" "$(printf '%s\n' "$floors" | sort -u)" "$floors"
check "...every one a full sha" "" "$(printf '%s\n' "$floors" | grep -vE '^[0-9a-f]{40}$')"
digest=$(printf '%s\n' "$floors" | { sha256sum 2>/dev/null || shasum -a 256; } | cut -c1-16)
check "the prefix names the runner's OS" "prefix=sbt-old-ref-deps-Linux-" "$(printf '%s\n' "$out" | grep '^prefix=')"
check "the key is the prefix and a digest of the floors alone" "key=sbt-old-ref-deps-Linux-$digest" \
  "$(printf '%s\n' "$out" | grep '^key=')"
check "...the same on another run" "$out" "$(RUNNER_OS=Linux bash "$REPO_ROOT/scripts/ci/old-ref-deps-key.sh")"

env_lines=$(JAVA_TOOL_OPTIONS="-XX:ActiveProcessorCount=4" bash "$REPO_ROOT/scripts/ci/jvm-offline-env.sh")
check "offline: the job's own JVM options kept, the network proxied to a refusing port" \
  "JAVA_TOOL_OPTIONS=-XX:ActiveProcessorCount=4 -Dhttp.proxyHost=127.0.0.1 -Dhttp.proxyPort=9 -Dhttps.proxyHost=127.0.0.1 -Dhttps.proxyPort=9" \
  "$(printf '%s\n' "$env_lines" | grep '^JAVA_TOOL_OPTIONS=')"
check "offline: sbt-guard retries no download (none can succeed)" "SBT_GUARD_DOWNLOAD_BACKOFF=" \
  "$(printf '%s\n' "$env_lines" | grep '^SBT_GUARD_DOWNLOAD_BACKOFF')"
check "offline with no JVM options of the job's: only the proxy" \
  "JAVA_TOOL_OPTIONS=-Dhttp.proxyHost=127.0.0.1 -Dhttp.proxyPort=9 -Dhttps.proxyHost=127.0.0.1 -Dhttps.proxyPort=9" \
  "$(env -u JAVA_TOOL_OPTIONS bash "$REPO_ROOT/scripts/ci/jvm-offline-env.sh" | grep '^JAVA_TOOL_OPTIONS=')"

# A real JVM under it: its fetch of the pom the 429 refused is refused locally, never asked of Maven Central.
if command -v java > /dev/null 2>&1 && java -version > /dev/null 2>&1; then
  cat > "$work/Fetch.java" <<'JAVA'
public class Fetch {
  public static void main(String[] args) throws Exception {
    try (var in = new java.net.URL(args[0]).openStream()) { System.out.println("fetched"); }
    catch (java.io.IOException e) { System.out.println(e.getClass().getSimpleName()); }
  }
}
JAVA
  options=$(printf '%s\n' "$env_lines" | sed -n 's/^JAVA_TOOL_OPTIONS=-XX:ActiveProcessorCount=4 //p')
  got=$(JAVA_TOOL_OPTIONS="$options" java "$work/Fetch.java" \
    https://repo1.maven.org/maven2/org/scala-sbt/sbt/1.13.0/sbt-1.13.0.pom 2>/dev/null | tail -1)
  check "a JVM under it cannot fetch from Maven Central: the connection is refused" "ConnectException" "$got"
else
  printf '  (no java here: the JVM check runs where one is)\n'
fi

spec_summary

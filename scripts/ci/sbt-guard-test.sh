#!/usr/bin/env bash
# scripts/ci/sbt-guard/sbt against a stand-in sbt: it passes arguments, output and status through, runs a batch once
# more (after `staleTastyDrop`) only when it failed on a stale TASTy read that the build's own retry did not take, and
# runs a command again after each backoff while it fails only on downloading a dependency from a remote repository
# (coursier's negative records forgotten first).
# Run: bash scripts/ci/sbt-guard-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m sbt-guard\n'

work="$(cd "$(mktemp -d)" && pwd -P)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"

# The stand-in: each call is logged; $FAKE_SBT_SCRIPT names, per call number, what it prints and how it exits.
cat > "$work/bin/sbt" <<'EOF'
#!/usr/bin/env bash
n=$(( $(cat "$FAKE_SBT_DIR/calls" 2>/dev/null || echo 0) + 1 ))
echo "$n" > "$FAKE_SBT_DIR/calls"
echo "$*" >> "$FAKE_SBT_DIR/args"
case "$(sed -n "${n}p" "$FAKE_SBT_DIR/script")" in
  ok)      echo "out $*"; echo "err $*" >&2; exit 0 ;;
  fail)    echo "[error] Compilation failed"; exit 1 ;;
  stale)   echo "[warn] Ignoring analyses of \$anonfun due to error in reading TASTy."; exit 1 ;;
  retried) echo "[warn] Ignoring analyses of x due to error in reading TASTy."
           echo "[warn] [stale TASTy] running the batch again: compile"; exit 1 ;;
  # lm-coursier's failures, as Main #2678 (a 404 from Central) and PR #275's run (a 429) printed them
  dl404)   echo "[error] sbt.librarymanagement.ResolveException: Error downloading io.sentry:sentry-logback:8.65.0"
           echo "[error]   Not found"
           echo "[error]   not found: /home/runner/.ivy2/localio.sentry/sentry-logback/8.65.0/ivys/ivy.xml"
           echo "[error]   not found: https://repo1.maven.org/maven2/io/sentry/sentry-logback/8.65.0/sentry-logback-8.65.0.pom"
           exit 1 ;;
  dl429)   echo "[error] sbt.librarymanagement.ResolveException: Error downloading io.sentry:sentry-logback:8.65.0"
           echo "[error]   download error: Caught java.io.IOException (Server returned HTTP response code: 429 for URL: https://repo1.maven.org/maven2/io/sentry/sentry-logback/8.65.0/sentry-logback-8.65.0.pom) while downloading"
           exit 1 ;;
  local)   echo "[error] sbt.librarymanagement.ResolveException: Error downloading com.example:missing:1.0"
           echo "[error]   not found: /home/runner/.ivy2/localcom.example/missing/1.0/ivys/ivy.xml"
           exit 1 ;;
  *)       exit 9 ;;
esac
EOF
chmod +x "$work/bin/sbt"
# The stand-in curl: the guard's HEAD request for a 429's Retry-After, answered from $work/headers (no network), the
# URL asked logged; and a stand-in sleep, logging how long the guard waited instead of waiting.
cat > "$work/bin/curl" <<'EOF'
#!/usr/bin/env bash
echo "${*: -1}" >> "$FAKE_SBT_DIR/curled"
cat "$FAKE_SBT_DIR/headers" 2>/dev/null
EOF
cat > "$work/bin/fake-sleep" <<'EOF'
#!/usr/bin/env bash
echo "$1" >> "$FAKE_SBT_DIR/slept"
EOF
chmod +x "$work/bin/curl" "$work/bin/fake-sleep"
export HOME="$work/home" FAKE_SBT_DIR="$work" COURSIER_CACHE="$work/coursier" SBT_GUARD_DOWNLOAD_BACKOFF="0 0" \
  SBT_GUARD_SLEEP="$work/bin/fake-sleep"

guard() {
  rm -f "$work/calls" "$work/args"
  printf '%s\n' "$@" > "$work/script"
  PATH="$REPO_ROOT/scripts/ci/sbt-guard:$work/bin:$PATH" sbt -batch 'set x := 1' compile > "$work/stdout" 2> "$work/stderr"
  echo $? > "$work/status"
}
calls() { cat "$work/calls" 2>/dev/null || echo 0; }

guard ok
check "a passing run: one call, status 0" "1 0" "$(calls) $(cat "$work/status")"
check "its arguments reach sbt unchanged" "-batch set x := 1 compile" "$(cat "$work/args")"
check "its stdout stays stdout" "out -batch set x := 1 compile" "$(cat "$work/stdout")"
check "its stderr stays stderr" "err -batch set x := 1 compile" "$(cat "$work/stderr")"

guard fail
check "an ordinary failure: one call, its status" "1 1" "$(calls) $(cat "$work/status")"

guard stale ok ok
check "a stale read the build did not retry: drop, then the batch again, passing" "3 0" "$(calls) $(cat "$work/status")"
check "the drop is staleTastyDrop, the rerun the same arguments" \
  "-batch staleTastyDrop|-batch set x := 1 compile" "$(sed -n '2,3p' "$work/args" | paste -sd'|' -)"

guard stale ok fail
check "a rerun that fails again keeps its failure" "3 1" "$(calls) $(cat "$work/status")"

guard retried
check "a stale read the build already retried: left failed, no second retry" "1 1" "$(calls) $(cat "$work/status")"

# A dependency Central failed to serve: lm-coursier retries neither a 404 nor a 429, so the guard does.
mkdir -p "$COURSIER_CACHE/v1/x"
: > "$COURSIER_CACHE/v1/x/.a-1.0.jar.error"; : > "$COURSIER_CACHE/v1/x/a-1.0.pom"
guard dl404 ok
check "a dependency Central answered 404 for: run again after the backoff, passing" "2 0" "$(calls) $(cat "$work/status")"
check "...the same arguments again" "-batch set x := 1 compile|-batch set x := 1 compile" "$(paste -sd'|' - < "$work/args")"
check "...coursier's negative records forgotten before it, the cache kept" "a-1.0.pom" \
  "$(find "$COURSIER_CACHE/v1/x" -mindepth 1 -exec basename {} \; | sort | paste -sd' ' -)"
check "...and it says why" "1" "$(grep -c 'sbt-guard: the run failed downloading a dependency' "$work/stderr")"
guard dl429 dl404 ok
check "a 429, then a 404: retried through both backoffs" "3 0" "$(calls) $(cat "$work/status")"
guard dl404 dl429 dl404
check "failing on every attempt: one more than its backoffs, then its own status" "3 1" "$(calls) $(cat "$work/status")"
guard local
check "a dependency no remote repository was asked for: not retried" "1 1" "$(calls) $(cat "$work/status")"

# A 429: the guard asks the refused URL for its Retry-After and waits at least that long, bounded.
slept() { paste -sd' ' - < "$work/slept" 2>/dev/null; }
rm -f "$work/slept" "$work/curled"; printf 'HTTP/2 429\r\nRetry-After: 45\r\n\r\n' > "$work/headers"
SBT_GUARD_DOWNLOAD_BACKOFF="30 60" guard dl429 dl429 ok
check "a 429 with Retry-After 45: waits 45 (over the 30 s step), then the 60 s step as is" "45 60" "$(slept)"
check "...passing on the third run" "3 0" "$(calls) $(cat "$work/status")"
check "...the Retry-After asked of the URL the 429 named" \
  "https://repo1.maven.org/maven2/io/sentry/sentry-logback/8.65.0/sentry-logback-8.65.0.pom" "$(sort -u "$work/curled")"
check "...and it says the host rate-limited it" "2" "$(grep -c 'sbt-guard: repo1.maven.org rate-limited the download (HTTP 429, Retry-After: 45' "$work/stderr")"
rm -f "$work/slept"; printf 'HTTP/2 429\r\nretry-after: 3600\r\n\r\n' > "$work/headers"
SBT_GUARD_DOWNLOAD_BACKOFF="30" SBT_GUARD_RETRY_AFTER_MAX=300 guard dl429 ok
check "a Retry-After past SBT_GUARD_RETRY_AFTER_MAX: waits the bound, not an hour" "300" "$(slept)"
rm -f "$work/slept"; : > "$work/headers"
SBT_GUARD_DOWNLOAD_BACKOFF="30" guard dl429 ok
check "a 429 with no Retry-After: the backoff step" "30" "$(slept)"
rm -f "$work/slept"; printf 'HTTP/2 429\r\nRetry-After: 5\r\n\r\n' > "$work/headers"
SBT_GUARD_DOWNLOAD_BACKOFF="30" guard dl429 ok
check "a Retry-After shorter than the step: the step" "30" "$(slept)"
rm -f "$work/slept" "$work/curled"
SBT_GUARD_DOWNLOAD_BACKOFF="30" guard dl404 ok
check "a 404 asks no Retry-After" "30 none" "$(slept) $([ -e "$work/curled" ] && echo asked || echo none)"
guard dl404 fail
check "a retry that fails otherwise keeps that failure, no further retry" "2 1" "$(calls) $(cat "$work/status")"
rm -f "$work/calls" "$work/args"
printf '%s\n' dl404 ok > "$work/script"
PATH="$REPO_ROOT/scripts/ci/sbt-guard:$work/bin:$PATH" SBT_GUARD_DOWNLOAD_BACKOFF="" sbt compile > /dev/null 2>&1
status=$?
check "SBT_GUARD_DOWNLOAD_BACKOFF empty: no retry" "1 1" "$(calls) $status"

rm -f "$work/calls" "$work/args"
printf '%s\n' stale ok ok > "$work/script"
PATH="$REPO_ROOT/scripts/ci/sbt-guard:$work/bin:$PATH" sbt --client 'worker/testOnly X' > /dev/null 2>&1
check "a --client run drops through its server, then runs again" \
  "--client worker/testOnly X|--client staleTastyDrop|--client worker/testOnly X" "$(paste -sd'|' - < "$work/args")"

rm -f "$work/calls"
PATH="$REPO_ROOT/scripts/ci/sbt-guard:/usr/bin:/bin" sbt compile > /dev/null 2>&1
check "no real sbt behind it: 127" "127" "$?"

spec_summary

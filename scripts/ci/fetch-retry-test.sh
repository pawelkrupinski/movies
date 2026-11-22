#!/usr/bin/env bash
# fetch-retry: a command failing FETCHING (a 429, a reset connection) runs again, up to its tries; failing otherwise
# (a compile error) it does not; a pass is a pass. A stub command counts its runs.
# Run: bash scripts/ci/fetch-retry-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
printf '\033[36m▸\033[0m fetch-retry\n'
tool="$REPO_ROOT/scripts/ci/fetch-retry"
work="$(mktemp -d)"; trap 'rm -rf "$work"' EXIT
cat > "$work/cmd" <<'EOF'
#!/usr/bin/env bash
n=$(( $(cat "$COUNT" 2> /dev/null || echo 0) + 1 )); echo "$n" > "$COUNT"
[ "$n" -ge "${PASS_AT:-99}" ] && { echo resolved; exit 0; }
echo "$SAYS"; exit 1
EOF
chmod +x "$work/cmd"
run() { rm -f "$work/count"; COUNT="$work/count" "$tool" --wait 0 "$@" > "$work/out" 2>&1; echo "$?|$(cat "$work/count")"; }

check "Maven Central's 429, then a pass: run again, and passes" "0|2" \
  "$(SAYS="Could not GET 'https://repo.maven.apache.org/x.jar'. Received status code 429 from server: Too Many Requests" PASS_AT=2 run -- "$work/cmd")"
check "a fetch failure every time: its tries, then the failure, naming the fetch" "1|3|1" \
  "$(SAYS="java.net.SocketException: Connection reset" run -- "$work/cmd")|$(grep -c '::error title=fetch-retry::the dependency fetch failed 3 times' "$work/out")"
check "...--tries bounds it" "1|2" "$(SAYS="Connection reset" run --tries 2 -- "$work/cmd")"
check "a failure that is no fetch (a compile error): once, never again" "1|1|1" \
  "$(SAYS="[error] Foo.scala:3: type mismatch" run -- "$work/cmd")|$(grep -c 'not retried' "$work/out")"
check "a pass: once" "0|1" "$(PASS_AT=1 run -- "$work/cmd")"
check "no command is a usage error" "64" "$("$tool" -- > /dev/null 2>&1; echo $?)"

spec_summary

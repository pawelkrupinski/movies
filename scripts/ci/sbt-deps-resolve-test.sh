#!/usr/bin/env bash
# sbt-deps-resolve.sh: retries a failed `sbt update` after each backoff, and fails once the attempts run out.
# Run: bash scripts/ci/sbt-deps-resolve-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m sbt-deps-resolve.sh\n'
script="$REPO_ROOT/scripts/ci/sbt-deps-resolve.sh"
dir="$(mktemp -d)"; trap 'rm -rf "$dir"' EXIT
mkdir -p "$dir/bin"
# A fake sbt that fails until its FAILS budget is spent, logging each call's arguments.
cat > "$dir/bin/sbt" <<'EOF'
#!/usr/bin/env bash
echo "$*" >> "$CALLS"
n=$(wc -l < "$CALLS")
[ "$n" -gt "$FAILS" ] || { echo "download error: Server returned HTTP response code: 429"; exit 1; }
EOF
chmod +x "$dir/bin/sbt"
export PATH="$dir/bin:$PATH" CALLS="$dir/calls" SBT_DEPS_BACKOFF="0 0 0"

run() { rm -f "$CALLS"; FAILS="$1" "$script" > "$dir/out" 2>&1; echo $?; }

check "resolves on the first attempt" "0" "$(run 0)"
check "...running sbt update once" "1 -Dsbt.supershell=false -Dsbt.color=false update" \
  "$(wc -l < "$CALLS" | tr -d ' ') $(cat "$CALLS")"
check "a 429 on two attempts is retried to success" "0" "$(run 2)"
check "...in three attempts" "3" "$(wc -l < "$CALLS" | tr -d ' ')"
check "every attempt failing fails the step" "1" "$(run 9)"
check "...after four attempts, one more than its three backoffs" "4" "$(wc -l < "$CALLS" | tr -d ' ')"
check "...naming them" "1" "$(grep -c '::error::sbt-deps: the build.s dependencies did not resolve in 4 attempts' "$dir/out")"
check "SBT_DEPS_BACKOFF sets how many retries" "2" "$(SBT_DEPS_BACKOFF=0 run 9 > /dev/null; wc -l < "$CALLS" | tr -d ' ')"

spec_summary

#!/usr/bin/env bash
# rerun-failed-sbt.sh in a scratch repository, against a stub `sbt` that records each rerun and
# — like the real thin client, which forwards the terminal to the server — reads its stdin.
# Run: bash scripts/ci/rerun-failed-sbt-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m rerun-failed-sbt.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
repo="$work/repo"; stub="$work/bin"
mkdir -p "$repo/worker/src/test/scala/a" "$repo/target/reports" "$stub"
git -C "$repo" init -q
printf 'package a\nclass OneSpec\n' > "$repo/worker/src/test/scala/a/OneSpec.scala"
printf 'package a\nclass TwoSpec\n' > "$repo/worker/src/test/scala/a/TwoSpec.scala"
for cls in a.OneSpec a.TwoSpec; do
  printf '<testsuite name="%s"><testcase classname="%s" name="works"><failure message="x"/></testcase></testsuite>\n' \
    "$cls" "$cls" > "$repo/target/reports/TEST-$cls.xml"
done

cat > "$stub/sbt" <<'STUB'
#!/usr/bin/env bash
[ "$2" = shutdown ] && exit 0
cat > /dev/null
printf '%s\n' "$2" >> "$STUB_LOG"
cls="$(cut -d' ' -f2 <<< "$2")"
printf '<testsuite name="%s"><testcase classname="%s" name="works"/></testsuite>\n' "$cls" "$cls" \
  > "$STUB_REPORTS/TEST-$cls.xml"
STUB
chmod +x "$stub/sbt"

export STUB_LOG="$work/log" STUB_REPORTS="$repo/target/reports"
: > "$STUB_LOG"
(cd "$repo" && PATH="$stub:$PATH" bash "$REPO_ROOT/scripts/ci/rerun-failed-sbt.sh" target/reports "unit tests" \
  > "$work/out" 2>&1)

check "every failed test is rerun three times, however much the client reads of stdin" \
  "3 3" "$(grep -c 'a.OneSpec' "$STUB_LOG") $(grep -c 'a.TwoSpec' "$STUB_LOG")"
check "a rerun names the module, the config and the one test" \
  'worker/Test/testOnly a.OneSpec -- -z "works"' "$(head -n 1 "$STUB_LOG")"

spec_summary

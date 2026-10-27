#!/usr/bin/env bash
# scripts/page-tests-local: boots the fixture server through sbt-warm --serve, runs CI's Playwright rows against its
# port, says a failing row and fails, and stops the server however it ends. Fakes stand in for sbt-warm, npx and npm.
# Run: bash scripts/page-tests-local-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
t=$(mktemp -d); trap 'rm -rf "$t"' EXIT
export CALLS="$t/calls" FIXTURE_POLL_SECONDS=0
# sbt-warm: --serve writes the port file named in its command and serves until killed; --stop is recorded
cat > "$t/sbt-warm" <<'STUB'
#!/usr/bin/env bash
case "$1" in
  --stop) echo "sbt-warm --stop" >> "$CALLS"; exit 0 ;;
  --serve) echo "sbt-warm --serve $2" >> "$CALLS"; port_file=${2##* }; echo 4321 > "$port_file"; exec sleep 300 ;;
esac
STUB
cat > "$t/npx" <<'STUB'
#!/usr/bin/env bash
echo "npx $* base=$KINOWO_BASE_URL workers=${PW_WORKERS:-}" >> "$CALLS"
case "$*" in *"${NPX_FAILS:-no such}"*) exit 1 ;; esac
STUB
printf '#!/usr/bin/env bash\necho "npm $*" >> "$CALLS"\n' > "$t/npm"
chmod +x "$t/sbt-warm" "$t/npx" "$t/npm"
printf '#!/usr/bin/env bash\necho "install $*" >> "$CALLS"\n' > "$t/install"
printf '#!/usr/bin/env bash\necho "node $*" >> "$CALLS"\n' > "$t/node"
chmod +x "$t/install" "$t/node"
export PAGE_TESTS_SBT_WARM="$t/sbt-warm" PAGE_TESTS_NPX="$t/npx" PAGE_TESTS_NPM="$t/npm" PAGE_TESTS_INSTALL="$t/install" PAGE_TESTS_NODE="$t/node"
unset CI PW_WORKERS

printf '\033[36m▸\033[0m page-tests-local\n'
: > "$CALLS"
out=$(bash "$REPO_ROOT/scripts/page-tests-local" 2>&1); code=$?
check "all rows pass: exit 0" "0" "$code"
check "the fixture server is FixtureServerMain, served by the kept sbt server" "1" \
  "$(grep -c 'sbt-warm --serve web/PageTest/runMain tools.FixtureServerMain .*/port' "$CALLS")"
check "every row runs against its port" "4 4" \
  "$(grep -c '^npx playwright test' "$CALLS") $(grep '^npx' "$CALLS" | grep -c 'base=http://127.0.0.1:4321')"
check "CI's chromium rows: three phones, the @agnostic set once, chrome-desktop; axe-core never" "1 1 1 0" \
  "$(grep -c "chromium-galaxy-s25\*.*--grep-invert axe-core|@agnostic" "$CALLS") $(grep -c 'chromium-galaxy-s10 --grep @agnostic' "$CALLS") $(grep -c 'project chrome-desktop' "$CALLS") $(grep '^npx' "$CALLS" | grep -vc 'grep-invert.*axe-core')"
check "the WebKit row runs one worker" "1" "$(grep -c 'webkit-desktop.*workers=1' "$CALLS")"
check "the server is stopped afterwards" "1" "$(grep -c 'sbt-warm --stop' "$CALLS")"
check "no serving process is left" "0" "$(pgrep -f "$t/sbt-warm --serve" | wc -l | tr -d ' ')"

: > "$CALLS"
bash "$REPO_ROOT/scripts/page-tests-local" chromium >/dev/null 2>&1
check "chromium runs the Chromium rows only" "3 0" "$(grep -c '^npx' "$CALLS") $(grep -c webkit "$CALLS")"

: > "$CALLS"
out=$(NPX_FAILS=chrome-desktop bash "$REPO_ROOT/scripts/page-tests-local" 2>&1); code=$?
check "a failing row fails the run, names the row, and the rows after it still run" "1 1 1" \
  "$code $(printf '%s\n' "$out" | grep -c '==> chrome desktop FAILED') $(grep -c webkit-desktop "$CALLS")"
check "...and stops the server" "1" "$(grep -c 'sbt-warm --stop' "$CALLS")"
check "off CI it installs, warms and pins nothing" "0 0" "$(grep -c '^install\|^node' "$CALLS") $(grep '^npx' "$CALLS" | grep -c 'workers=5')"
: > "$CALLS"
CI=true bash "$REPO_ROOT/scripts/page-tests-local" chromium >/dev/null 2>&1
check "on CI: the row's browsers installed and warmed, five workers" "1 1 3" \
  "$(grep -cx 'install chromium' "$CALLS") $(grep -cx 'node warm-browsers.mjs chromium' "$CALLS") $(grep '^npx' "$CALLS" | grep -c 'workers=5$')"
: > "$CALLS"
CI=true bash "$REPO_ROOT/scripts/page-tests-local" >/dev/null 2>&1
check "on CI, all: both engines; the WebKit row still one worker" "1 1" \
  "$(grep -cx 'install chromium webkit' "$CALLS") $(grep -c 'webkit-desktop.*workers=1$' "$CALLS")"
check "an unknown row is a usage error" "2" "$(bash "$REPO_ROOT/scripts/page-tests-local" firefox >/dev/null 2>&1; echo $?)"
spec_summary

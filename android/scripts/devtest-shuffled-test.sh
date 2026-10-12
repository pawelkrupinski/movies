#!/usr/bin/env bash
#
# devtest-shuffled.sh over a scratch copy of its layout, with a stub devtest.sh in
# place of the emulator run, so it runs in a second with no SDK. The stub reads
# its stdin the way adb and Gradle's client can: a run must still reach every
# class, not just the first.
#
#   android/scripts/devtest-shuffled-test.sh
#
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
. "$HERE/../../scripts/shell-spec.sh"

printf '\033[36m▸\033[0m devtest-shuffled.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/android/scripts" "$work/scripts" "$work/android/app/src/androidTest/java/a"
cp "$HERE/devtest-shuffled.sh" "$work/android/scripts/"
cp "$HERE/../../scripts/seeded-order.sh" "$work/scripts/"
for c in One Two Three; do printf 'class %s { @Test fun t() {} }\n' "$c" > "$work/android/app/src/androidTest/java/a/$c.kt"; done
cat > "$work/android/scripts/devtest.sh" <<'STUB'
#!/usr/bin/env bash
cat >/dev/null
echo "${@: -1}" >> "$STUB_LOG"
STUB
chmod +x "$work/android/scripts/"*.sh

export STUB_LOG="$work/log"
: > "$STUB_LOG"
bash "$work/android/scripts/devtest-shuffled.sh" 7 >/dev/null 2>&1
check "every class runs alone, though a run reads stdin" "a.One a.Three a.Two" \
  "$(sort "$STUB_LOG" | tr '\n' ' ' | sed 's/ $//')"

# Each run writes its report to the SAME Gradle paths, so a failing class's report is
# overwritten by the next class's unless the harness keeps it.
cat > "$work/android/scripts/devtest.sh" <<'STUB'
#!/usr/bin/env bash
cat >/dev/null
class="${@: -1}"
app="$(cd "$(dirname "${BASH_SOURCE[0]}")/../app" && pwd)"
rm -rf "$app/build/reports/androidTests/connected" "$app/build/outputs/androidTest-results/connected"
mkdir -p "$app/build/reports/androidTests/connected" "$app/build/outputs/androidTest-results/connected"
echo "report of $class" > "$app/build/reports/androidTests/connected/index.html"
echo "result of $class" > "$app/build/outputs/androidTest-results/connected/TEST.xml"
[ "$class" != a.Two ]
STUB
chmod +x "$work/android/scripts/devtest.sh"
bash "$work/android/scripts/devtest-shuffled.sh" 7 >/dev/null 2>&1
kept="$work/android/app/build/order-independence"
check "a failing class keeps its own report" "report of a.Two" "$(cat "$kept/a.Two/reports/index.html" 2>/dev/null)"
check "a failing class keeps its own result XML" "result of a.Two" "$(cat "$kept/a.Two/results/TEST.xml" 2>/dev/null)"
check "only failing classes are kept" "a.Two" "$(cd "$kept" 2>/dev/null && find . -mindepth 1 -maxdepth 1 | sed 's|^\./||' | sort | tr '\n' ' ' | sed 's/ $//')"

spec_summary

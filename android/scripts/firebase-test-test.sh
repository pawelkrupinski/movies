#!/usr/bin/env bash
# firebase-test.sh against a stub gcloud and gradlew, on /bin/bash (3.2 on a Mac): the gcloud command it execs for
# the default run, the whole suite (FULL=1) and a blocking run (WAIT=1) -- the last two leave an array empty,
# which `set -u` calls unbound there.
# Run: bash android/scripts/firebase-test-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m firebase-test.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
# A copy of the script inside a fake android/ tree: it cds to its own parent and builds with ./gradlew.
mkdir -p "$work/android/scripts" "$work/android/app/build/outputs/apk/debug" \
  "$work/android/app/build/outputs/apk/androidTest/debug" "$work/bin"
cp "$REPO_ROOT/android/scripts/firebase-test.sh" "$work/android/scripts/"
: > "$work/android/app/build/outputs/apk/debug/app-debug.apk"
: > "$work/android/app/build/outputs/apk/androidTest/debug/app-debug-androidTest.apk"
printf '#!/bin/sh\nexit 0\n' > "$work/android/gradlew"
cat > "$work/bin/gcloud" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG"
exit 0
STUB
chmod +x "$work/android/gradlew" "$work/bin/gcloud"
export STUB_LOG="$work/log"

# run <env assignments...>: the gcloud call that submits the run, one line ("" when none); the status in $work/status
run() {
  : > "$STUB_LOG"
  env PATH="$work/bin:$PATH" MODEL=cactus VERSION=27 "$@" /bin/bash "$work/android/scripts/firebase-test.sh" > "$work/out" 2>&1
  echo "$?" > "$work/status"
  grep '^firebase test android run' "$STUB_LOG"
}

check "the default run is async and targets the deep-link test" \
  "firebase test android run --async --type instrumentation --app app/build/outputs/apk/debug/app-debug.apk --test app/build/outputs/apk/androidTest/debug/app-debug-androidTest.apk --device model=cactus,version=27,locale=pl,orientation=portrait --timeout 5m --test-targets class pl.kinowo.DeepLinkInstrumentedTest" \
  "$(run X=1)"
check "FULL=1 runs the whole androidTest suite: no --test-targets, and succeeds" "0:0" \
  "$(run FULL=1 >/dev/null; echo "$(cat "$work/status"):$(grep -c -- '--test-targets' "$STUB_LOG")")"
check "WAIT=1 blocks for the verdict: no --async, and succeeds" "0:0" \
  "$(run WAIT=1 >/dev/null; echo "$(cat "$work/status"):$(grep -c -- '--async' "$STUB_LOG")")"

spec_summary

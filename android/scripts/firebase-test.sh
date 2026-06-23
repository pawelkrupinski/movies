#!/usr/bin/env bash
#
# Run the on-device deep-link test on a real Xiaomi/Redmi in Firebase Test Lab.
#
# Why: the deep-link "opens app but not the film page" bug only reproduced on
# MIUI (which keeps the app warm with the previous city loaded). This submits
# the hermetic DeepLinkInstrumentedTest to a physical Xiaomi device so we verify
# the fix on real MIUI hardware, not just an AOSP emulator.
#
# Prereqs (one-time):
#   - gcloud CLI on a Python 3.10–3.14 (this script auto-points CLOUDSDK_PYTHON
#     at python3.14/3.13/… if you don't already export one — gcloud's bundled
#     launcher rejects Python 3.9).
#   - gcloud auth login && gcloud config set project <your-firebase-project>
#   - gcloud services enable testing.googleapis.com toolresults.googleapis.com
#
# Usage:
#   android/scripts/firebase-test.sh                 # auto-pick a Xiaomi device
#   MODEL=<id> VERSION=<api> android/scripts/firebase-test.sh   # pin a device
#   FULL=1 android/scripts/firebase-test.sh          # whole androidTest suite
#
set -euo pipefail

cd "$(dirname "$0")/.."   # android/

# gcloud needs Python 3.10–3.14; bundled launcher dies on 3.9.
if [[ -z "${CLOUDSDK_PYTHON:-}" ]]; then
  for v in 3.14 3.13 3.12 3.11 3.10; do
    p="$(command -v "python$v" || true)"
    if [[ -n "$p" ]]; then export CLOUDSDK_PYTHON="$p"; break; fi
  done
fi
echo "CLOUDSDK_PYTHON=${CLOUDSDK_PYTHON:-<gcloud default>}"

command -v gcloud >/dev/null || { echo "gcloud not installed: https://cloud.google.com/sdk/docs/install"; exit 1; }
gcloud config get-value project >/dev/null 2>&1 || { echo "No project set: gcloud config set project <id>"; exit 1; }

echo "==> Building app + test APKs"
./gradlew :app:assembleDebug :app:assembleDebugAndroidTest -q

APP_APK="app/build/outputs/apk/debug/app-debug.apk"
TEST_APK="app/build/outputs/apk/androidTest/debug/app-debug-androidTest.apk"
[[ -f "$APP_APK" && -f "$TEST_APK" ]] || { echo "APKs missing after build"; exit 1; }

# Resolve a physical Xiaomi/Redmi device + a supported version, unless pinned.
MODEL="${MODEL:-}"
VERSION="${VERSION:-}"
if [[ -z "$MODEL" ]]; then
  echo "==> Resolving a physical Xiaomi device from Test Lab"
  # Columns: id <tab> supportedVersionIds(list). Pick the first Xiaomi PHYSICAL
  # model and its highest API level.
  read -r MODEL VERSION < <(
    gcloud firebase test android models list \
      --filter="brand=Xiaomi AND form=PHYSICAL" \
      --format="value(id, supportedVersionIds.list(separator=','))" 2>/dev/null \
    | head -1 \
    | awk -F'\t' '{n=split($2,a,","); max=a[1]; for(i=2;i<=n;i++) if(a[i]+0>max+0) max=a[i]; print $1, max}'
  )
fi
[[ -n "$MODEL" && -n "$VERSION" ]] || {
  echo "Could not auto-resolve a Xiaomi device. Available Xiaomi models:"
  gcloud firebase test android models list --filter="brand=Xiaomi AND form=PHYSICAL" --format="table(id,name,supportedVersionIds)" || true
  echo "Re-run with: MODEL=<id> VERSION=<api> $0"
  exit 1
}
echo "==> Device: model=$MODEL version=$VERSION"

TARGETS=(--test-targets "class pl.kinowo.DeepLinkInstrumentedTest")
[[ "${FULL:-}" == "1" ]] && TARGETS=()

exec gcloud firebase test android run \
  --type instrumentation \
  --app "$APP_APK" \
  --test "$TEST_APK" \
  --device "model=$MODEL,version=$VERSION,locale=pl,orientation=portrait" \
  --timeout 5m \
  "${TARGETS[@]}"

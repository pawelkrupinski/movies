#!/usr/bin/env bash
# scripts/floor-bash: which bash is the floor -- a system bash 3 (macOS's /bin/bash), else the kept 3.2.57 build --
# and a host with neither (Linux: ci-runner's landing checks) answering exit 1 with the fix, never another bash. The
# system bash and the build are stubs reporting the version given; --install's real build (~1 min) is not run here:
# the stub `cc` it finds missing, or a tarball failing its checksum, stops it before anything is built.
# Run: bash scripts/floor-bash-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m floor-bash\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
fake_bash() { # <path> <major.minor.patch>: a "bash" whose BASH_VERSINFO reads as given
  mkdir -p "$(dirname "$1")"
  printf '#!/bin/sh\necho %s\n' "$2" > "$1"
  chmod +x "$1"
}
# [env <assignments> <bash> <floor-bash> <args>] -- with nothing given, scripts/floor-bash itself
floor() {
  [ $# -gt 0 ] || set -- env bash "$REPO_ROOT/scripts/floor-bash"
  FLOOR_BASH_SYSTEM="$work/system-bash" FLOOR_BASH_DIR="$work/build" "$@" 2> "$work/err"
}

fake_bash "$work/system-bash" 3.2.57
check "a system bash 3 (macOS's /bin/bash) is the floor" "$work/system-bash" "$(floor)"

fake_bash "$work/system-bash" 5.2.21
check "a bash 5 (Linux) is never the floor: exit 1, naming --install" "1 1" \
  "$(floor > /dev/null; echo $?) $(grep -c 'scripts/floor-bash --install' "$work/err")"
fake_bash "$work/build/bin/bash" 3.2.57
check "...the kept 3.2.57 build is" "$work/build/bin/bash" "$(floor)"
fake_bash "$work/build/bin/bash" 3.2.48
check "...a build of another 3.2 is not" "1" "$(floor > /dev/null; echo $?)"
rm -rf "$work/build"

# --install stops before building when a tool is missing: a PATH with no cc
mkdir -p "$work/nocc"
for tool in make curl mktemp tar sha256sum dirname getconf rm; do
  p="$(command -v "$tool")" && ln -sf "$p" "$work/nocc/$tool"
done
check "--install with no cc fails, naming it" "1 1" \
  "$(floor env PATH="$work/nocc" "$BASH" "$REPO_ROOT/scripts/floor-bash" --install > /dev/null; echo $?) $(grep -c 'needs cc' "$work/err")"
check "an unknown argument is a usage error" "64" "$(floor env "$BASH" "$REPO_ROOT/scripts/floor-bash" --nope > /dev/null; echo $?)"

# The floor is one thing everywhere: the hook asks scripts/floor-bash, and so does the nightly macOS step.
# shellcheck disable=SC2016 # the workflow's text, not expanded
check "order-independence.yml's floor step runs the self-tests under scripts/floor-bash's bash" "1" \
  "$(grep -c 'scripts/run-script-tests --bash "$(scripts/floor-bash)"' "$REPO_ROOT/.github/workflows/order-independence.yml")"
check "the landing check's tools build it (.github/actions/install-floor-bash, from hook-tools)" "1" \
  "$(grep -c 'uses: ./.github/actions/install-floor-bash' "$REPO_ROOT/.github/actions/hook-tools/action.yml")"

spec_summary

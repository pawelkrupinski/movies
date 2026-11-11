#!/usr/bin/env bash
# install-swift.sh against a local stand-in for download.swift.org (a file:// base serving a stub toolchain tarball).
# Run: bash scripts/ci/install-swift-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m install-swift.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
install="$REPO_ROOT/scripts/ci/install-swift.sh"

# download.swift.org's layout for 9.9.9 on ubuntu22.04, holding a toolchain whose `swift` says its version.
name="swift-9.9.9-RELEASE-ubuntu22.04"
mkdir -p "$work/src/$name/usr/bin" "$work/site/swift-9.9.9-release/ubuntu2204/swift-9.9.9-RELEASE"
printf '#!/bin/sh\necho "Swift version 9.9.9 (stub)"\n' > "$work/src/$name/usr/bin/swift"
chmod +x "$work/src/$name/usr/bin/swift"
tarball="$work/site/swift-9.9.9-release/ubuntu2204/swift-9.9.9-RELEASE/$name.tar.gz"
tar -czf "$tarball" -C "$work/src" "$name"
sha=$( (sha256sum "$tarball" 2>/dev/null || shasum -a 256 "$tarball") | cut -d' ' -f1)

# run <sha256>: installs into a fresh tool directory; leaves the exit status in $status, the output in $work/log
run() {
  rm -rf "$work/tools"; : > "$work/path"
  SWIFT_DOWNLOAD_BASE="file://$work/site" SWIFT_INSTALL_DIR="$work/tools" GITHUB_PATH="$work/path" \
    bash "$install" 9.9.9 ubuntu22.04 "$1" > "$work/log" 2>&1
  status=$?
}
logged() { grep -q -- "$1" "$work/log" && echo yes || echo no; }

run "$sha"
check "a tarball matching the pinned SHA-256 installs, its usr/bin put on the job's PATH" "0|$work/tools/$name/usr/bin" \
  "$status|$(cat "$work/path")"
check "…and the installed swift answers" "yes" "$(logged 'Swift version 9.9.9 (stub)')"
check "…with no keyserver asked: nothing touches gpg" "no" "$(logged 'gpg')"

run "0000000000000000000000000000000000000000000000000000000000000000"
check "a tarball not matching the pin fails the step by name, and installs nothing" "1|yes|" \
  "$status|$(logged '::error::.*SHA-256')|$(cat "$work/path")"

rm -rf "$work/tools"; : > "$work/path"
SWIFT_DOWNLOAD_BASE="file://$work/missing" SWIFT_INSTALL_DIR="$work/tools" GITHUB_PATH="$work/path" \
  bash "$install" 9.9.9 ubuntu22.04 "$sha" > "$work/log" 2>&1
status=$?
check "a download that fails ends the step naming the URL" "1|yes" "$status|$(logged '::error::could not download')"

status=$(bash "$install" 9.9.9 >/dev/null 2>&1; echo $?)
check "a missing argument is a usage error" "64" "$status"

spec_summary

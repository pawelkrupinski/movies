#!/usr/bin/env bash
#
# install-swift.sh <version> <platform> <sha256>
#
# Installs swift.org's Linux toolchain <version> for <platform> (e.g. 6.2.1 ubuntu22.04) and puts its usr/bin on the
# job's PATH ($GITHUB_PATH). The tarball is checked against the SHA-256 the workflow pins, not against swift.org's
# signing keys refreshed from a keyserver: swift-actions/setup-swift@v2 refreshes them from hkp://keyserver.ubuntu.com
# on every toolchain download and aborts when that keyserver fails (Main #2388 and #2390, 2026-11-11), so an outside
# keyserver held Main red. The pin was taken from a tarball whose swift.org signature verified once (see ci.yml).
#
# SWIFT_DOWNLOAD_BASE (default https://download.swift.org) and SWIFT_INSTALL_DIR (default $RUNNER_TOOL_CACHE/swift)
# exist for install-swift-test.sh.
set -euo pipefail

[ "$#" -eq 3 ] || { echo "usage: $0 <version> <platform, e.g. ubuntu22.04> <sha256>" >&2; exit 64; }
version="$1" platform="$2" expected="$3"
base="${SWIFT_DOWNLOAD_BASE:-https://download.swift.org}"
dest="${SWIFT_INSTALL_DIR:-${RUNNER_TOOL_CACHE:-/opt/hostedtoolcache}/swift}"
name="swift-$version-RELEASE-$platform"
url="$base/swift-$version-release/${platform//./}/swift-$version-RELEASE/$name.tar.gz"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
tarball="$work/$name.tar.gz"

echo "downloading $url"
curl -sSfL --retry 3 --retry-delay 5 "$url" -o "$tarball" ||
  { echo "::error::could not download Swift $version for $platform from $url"; exit 1; }

actual=$( (sha256sum "$tarball" 2>/dev/null || shasum -a 256 "$tarball") | cut -d' ' -f1)
if [ "$actual" != "$expected" ]; then
  echo "::error::Swift $version tarball's SHA-256 is $actual, not the pinned $expected -- re-verify swift.org's signature before re-pinning"
  exit 1
fi

mkdir -p "$dest"
tar -xzf "$tarball" -C "$dest"
bin="$dest/$name/usr/bin"
[ -x "$bin/swift" ] || { echo "::error::$name has no usr/bin/swift"; exit 1; }
echo "$bin" >> "$GITHUB_PATH"
"$bin/swift" --version

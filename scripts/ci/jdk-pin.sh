#!/usr/bin/env bash
# The pinned Temurin JDK build for <java-version> on this runner's platform, as GITHUB_OUTPUT lines (file, url,
# sha256, version, key) for .github/actions/setup-jdk, which keeps that tarball in ONE actions/cache entry per platform
# and installs it with setup-java's `jdkfile`; prints nothing for an unpinned version or platform, which setup-jdk
# leaves to plain setup-java. setup-java's own resolution downloads the latest build from GitHub's release CDN on
# every run, and its 504s (2026-11-08 08:54-10:00 UTC) failed Main #2306's integration-test (identity) and
# page-tests-safari; setup-java's `cache` input caches build-tool dependencies, never the JDK.
# Usage: scripts/ci/jdk-pin.sh <java-version>   (env RUNNER_OS, RUNNER_ARCH, RUNNER_TOOL_CACHE as GitHub sets them)
#
# A runner image whose tool-cache already holds a Temurin <java-version> (github.com's ubuntu-26.04 carries 27.0.0+35)
# prints nothing too: plain setup-java resolves that from the tool-cache with no network at all, where the pin would
# be a download (precommit run 42469475456's 504s, on runners that had a JDK 27 the whole time).
#
# A bump: the new release name and each platform's checksum from
#   curl -s "https://api.adoptium.net/v3/assets/latest/27/hotspot?os=linux&architecture=x64&image_type=jdk"
#   (`.[].release_name`, `.[].binary.package.checksum`; os=mac&architecture=aarch64, os=linux&architecture=aarch64).
# Tested by scripts/ci/jdk-pin-test.sh.
set -euo pipefail

java_version="${1:?usage: jdk-pin.sh <java-version>}"
[ "$java_version" = 27 ] || exit 0
for preinstalled in "${RUNNER_TOOL_CACHE:-/nonexistent}/Java_Temurin-Hotspot_jdk/$java_version".*; do
  if [ -d "$preinstalled" ]; then
    echo "jdk-pin: the runner's tool-cache holds $(basename "$preinstalled") -- setup-java takes it, no download" >&2
    exit 0
  fi
done

release=jdk-27.0.1.0.2+1
build=27.0.1.0.2_1
case "${RUNNER_OS:-}/${RUNNER_ARCH:-}" in
  Linux/X64) platform=x64_linux sha256=30cb129f6cf299c852cec0feca9ea2a596d2ee9f26d8605d3a253951f6c8b20f ;;
  Linux/ARM64) platform=aarch64_linux sha256=5ddacf925244a00b60cac828b61ff2f67a68cd32b8dfe5cee1405e333b4589e5 ;;
  macOS/ARM64) platform=aarch64_mac sha256=c2619cb73ff1d296edd0644d92ae8ed94039bc1bdd43282398835b7f88134fb6 ;;
  *) exit 0 ;;
esac
file="OpenJDK27U-jdk_${platform}_hotspot_${build}.tar.gz"
echo "file=$file"
echo "url=https://github.com/adoptium/temurin27-binaries/releases/download/${release//+/%2B}/$file"
echo "sha256=$sha256"
echo "version=27.0.1"
echo "key=jdk-temurin-${release}-${platform}-${sha256}"

#!/usr/bin/env bash
# The pinned Temurin JDK build for <java-version> on this runner's platform, as GITHUB_OUTPUT lines (file, url,
# sha256, version, key, and fallback_file, fallback_url, fallback_sha256: the same release's Azul Zulu build from
# cdn.azul.com, which setup-jdk fetches -- verified the same way, never cached -- only when GitHub's release CDN
# fails the download for minutes on end, as it did from 08:54 to past 11:00 UTC on 2026-11-08) for .github/actions/setup-jdk, which keeps that tarball in ONE actions/cache entry per platform
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
#   (`.[].release_name`, `.[].binary.package.checksum`; os=mac&architecture=aarch64, os=linux&architecture=aarch64),
# and the Zulu build of the same release from
#   curl -s "https://api.azul.com/metadata/v1/zulu/packages/?java_version=27.0.1&os=linux&arch=x64&archive_type=tar.gz&java_package_type=jdk&javafx_bundled=false&crac_supported=false&latest=true&release_status=ga&availability_types=CA"
#   (its `package_uuid`, then `.sha256_hash` of https://api.azul.com/metadata/v1/zulu/packages/<uuid>; os=macos).
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
zulu=zulu27.30.109-ca-jdk27.0.1.0.3
case "${RUNNER_OS:-}/${RUNNER_ARCH:-}" in
  Linux/X64)
    platform=x64_linux sha256=30cb129f6cf299c852cec0feca9ea2a596d2ee9f26d8605d3a253951f6c8b20f
    zulu_platform=linux_x64 zulu_sha256=c128817926e4eebb30868b6ba082028f493afa716e459e34049bde0ab49a9829 ;;
  Linux/ARM64)
    platform=aarch64_linux sha256=5ddacf925244a00b60cac828b61ff2f67a68cd32b8dfe5cee1405e333b4589e5
    zulu_platform=linux_aarch64 zulu_sha256=4b4d2fb4a6bff5fa4d3a1f22ac5933c87d83e41d5f78bcd88df878326342d507 ;;
  macOS/ARM64)
    platform=aarch64_mac sha256=c2619cb73ff1d296edd0644d92ae8ed94039bc1bdd43282398835b7f88134fb6
    zulu_platform=macosx_aarch64 zulu_sha256=88dec2f8609e7c4162778cbaef5437f00febd3c498a7c0e77277d1f4ac8f839d ;;
  *) exit 0 ;;
esac
file="OpenJDK27U-jdk_${platform}_hotspot_${build}.tar.gz"
echo "file=$file"
echo "url=https://github.com/adoptium/temurin27-binaries/releases/download/${release//+/%2B}/$file"
echo "sha256=$sha256"
echo "version=27.0.1"
echo "key=jdk-temurin-${release}-${platform}-${sha256}"
echo "fallback_file=$zulu-$zulu_platform.tar.gz"
echo "fallback_url=https://cdn.azul.com/zulu/bin/$zulu-$zulu_platform.tar.gz"
echo "fallback_sha256=$zulu_sha256"

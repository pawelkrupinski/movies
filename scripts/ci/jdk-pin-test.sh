#!/usr/bin/env bash
# jdk-pin.sh per platform, and the pin's checksum gate through fetch-verified.sh against a stub `curl`.
# Run: bash scripts/ci/jdk-pin-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m jdk-pin.sh\n'

tool_cache="$(mktemp -d)"
pin() { RUNNER_OS="$1" RUNNER_ARCH="$2" RUNNER_TOOL_CACHE="${PIN_TOOL_CACHE:-$tool_cache}" bash "$REPO_ROOT/scripts/ci/jdk-pin.sh" "$3" 2> /dev/null; }
field() { sed -n "s/^$1=//p"; }

check "Linux x64's 27 is the pinned Temurin tarball" \
  "https://github.com/adoptium/temurin27-binaries/releases/download/jdk-27.0.1.0.2%2B1/OpenJDK27U-jdk_x64_linux_hotspot_27.0.1.0.2_1.tar.gz" \
  "$(pin Linux X64 27 | field url)"
check "...the file is the URL's last part" "OpenJDK27U-jdk_x64_linux_hotspot_27.0.1.0.2_1.tar.gz" "$(pin Linux X64 27 | field file)"
check "...setup-java's jdkfile gets the release's version" "27.0.1" "$(pin Linux X64 27 | field version)"
check "...the cache key names the release, platform and checksum" \
  "jdk-temurin-jdk-27.0.1.0.2+1-x64_linux-30cb129f6cf299c852cec0feca9ea2a596d2ee9f26d8605d3a253951f6c8b20f" \
  "$(pin Linux X64 27 | field key)"
check "macOS arm64 (macos-latest, page-tests-safari) has its own tarball" \
  "OpenJDK27U-jdk_aarch64_mac_hotspot_27.0.1.0.2_1.tar.gz" "$(pin macOS ARM64 27 | field file)"
check "...and checksum" "c2619cb73ff1d296edd0644d92ae8ed94039bc1bdd43282398835b7f88134fb6" "$(pin macOS ARM64 27 | field sha256)"
check "Linux arm64 is pinned" "OpenJDK27U-jdk_aarch64_linux_hotspot_27.0.1.0.2_1.tar.gz" "$(pin Linux ARM64 27 | field file)"
check "...the three platforms' keys differ" "3" \
  "$( { pin Linux X64 27; pin Linux ARM64 27; pin macOS ARM64 27; } | field key | sort -u | wc -l | tr -d ' ')"
check "the fallback is the same release's Zulu build, off cdn.azul.com, not GitHub" \
  "https://cdn.azul.com/zulu/bin/zulu27.30.109-ca-jdk27.0.1.0.3-linux_x64.tar.gz" "$(pin Linux X64 27 | field fallback_url)"
check "...checksum-pinned too" "c128817926e4eebb30868b6ba082028f493afa716e459e34049bde0ab49a9829" \
  "$(pin Linux X64 27 | field fallback_sha256)"
check "...per platform" "zulu27.30.109-ca-jdk27.0.1.0.3-macosx_aarch64.tar.gz zulu27.30.109-ca-jdk27.0.1.0.3-linux_aarch64.tar.gz" \
  "$(pin macOS ARM64 27 | field fallback_file) $(pin Linux ARM64 27 | field fallback_file)"
check "an unpinned version prints nothing (plain setup-java)" "" "$(pin Linux X64 17)"
check "an unpinned platform prints nothing" "" "$(pin Windows X64 27)"
mkdir -p "$tool_cache/preinstalled/Java_Temurin-Hotspot_jdk/27.0.0-35/x64"
check "a runner whose tool-cache holds a Temurin 27 prints nothing (setup-java takes it, no network)" "" \
  "$(PIN_TOOL_CACHE="$tool_cache/preinstalled" pin Linux X64 27)"
mkdir -p "$tool_cache/other/Java_Temurin-Hotspot_jdk/21.0.9-10/x64"
check "...but another major's does not count" "OpenJDK27U-jdk_x64_linux_hotspot_27.0.1.0.2_1.tar.gz" \
  "$(PIN_TOOL_CACHE="$tool_cache/other" pin Linux X64 27 | field file)"

printf '\033[36m▸\033[0m the pinned JDK through fetch-verified.sh\n'
work="$(mktemp -d)"
trap 'rm -rf "$work" "$tool_cache"' EXIT
mkdir -p "$work/bin"
cat > "$work/bin/curl" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG"
out=""; while [ $# -gt 0 ]; do [ "$1" = "-o" ] && { out="$2"; shift; }; shift; done
echo 'not the JDK' > "$out"
STUB
chmod +x "$work/bin/curl"
pin_file=$(pin Linux X64 27 | field file)
pin_url=$(pin Linux X64 27 | field url)
pin_sha256=$(pin Linux X64 27 | field sha256)
PATH="$work/bin:$PATH" STUB_LOG="$work/curls" \
  bash "$REPO_ROOT/scripts/ci/fetch-verified.sh" "$work/jdk/$pin_file" "$pin_url" "$pin_sha256" > "$work/out" 2>&1
check "a JDK download that is not the pinned build fails" "1" "$?"
check "...from the pinned URL" "1" "$(grep -c -- "$pin_url" "$work/curls")"
check "...and is not left for the cache to save" "absent" "$( [ -e "$work/jdk/$pin_file" ] && echo present || echo absent)"

printf '\033[36m▸\033[0m setup-jdk installs the pin from its cache\n'
action="$REPO_ROOT/.github/actions/setup-jdk/action.yml"
check "the pinned path restores and saves one cache path, fetches it (or its fallback) verified and installs it with jdkfile" "2 2 1" \
  "$(grep -c 'path: ${{ runner.temp }}/jdk-archive$' "$action") $(grep -c 'run: .*scripts/ci/fetch-verified.sh' "$action") $(grep -c 'distribution: jdkfile' "$action")"
check "...the save runs on main only, and only for the verified Temurin download, never the fallback" "1" \
  "$(grep -c "if: steps.pin.outputs.key != '' && github.ref == 'refs/heads/main' && .*steps.fetch.outcome == 'success'" "$action")"
check "...the Zulu fallback is fetched (verified) only when the Temurin fetch failed" "1" \
  "$(grep -c "if: steps.pin.outputs.key != '' && steps.fetch.outcome == 'failure'" "$action")"
check "...the Temurin fetch may fail without failing the job" "1" \
  "$(grep -A3 'id: fetch$' "$action" | grep -c 'continue-on-error: true')"

spec_summary

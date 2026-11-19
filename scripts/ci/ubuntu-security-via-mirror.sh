#!/usr/bin/env bash
# Route the GitHub-hosted Ubuntu runner's security.ubuntu.com apt sources to the archive mirror the image already uses,
# before an `apt-get update`. Usage: scripts/ci/ubuntu-security-via-mirror.sh
#
# The 26.04 image lists security.ubuntu.com (http and https, for -security and -updates) beside its own archive mirror
# (us-east-2.ec2.archive.ubuntu.com, azure.archive.ubuntu.com, ...). apt took the security host's copy of new -updates
# packages, and when that host refused connections and answered 503 for ~10 min, all three Playwright install attempts
# failed "Unable to fetch some archives" (Main #2680, page-tests-safari, 2026-11-19), though the mirror held the same
# packages. Every Ubuntu archive mirror carries every pocket, -security included, so the security host is not needed.
#
# The mirror is the first archive.ubuntu.com-family URI the sources name; with none, nothing changes (apt keeps the
# sources it has). Env: APT_SOURCES_DIR (default /etc/apt/sources.list.d), APT_SOURCES_LIST (default
# /etc/apt/sources.list), APT_SOURCES_SUDO (default sudo; empty runs without).
# Tested by scripts/ci/ubuntu-security-via-mirror-test.sh.
set -euo pipefail

dir="${APT_SOURCES_DIR:-/etc/apt/sources.list.d}"
main_list="${APT_SOURCES_LIST:-/etc/apt/sources.list}"
as_root=${APT_SOURCES_SUDO-sudo}
security='https?://security\.ubuntu\.com/ubuntu'

shopt -s nullglob
files=()
for file in "$main_list" "$dir"/*.list "$dir"/*.sources; do [ -f "$file" ] && files+=("$file"); done
[ "${#files[@]}" -gt 0 ] || exit 0

# The image's own archive mirror, as `scheme://host/ubuntu`.
mirror="$(grep -h -o -E 'https?://([a-z0-9-]+\.)*archive\.ubuntu\.com/ubuntu' "${files[@]}" 2>/dev/null | head -1 || true)"
if [ -z "$mirror" ]; then
  echo "ubuntu-security-via-mirror: no archive mirror in the apt sources; security.ubuntu.com left as it is"
  exit 0
fi

for file in "${files[@]}"; do
  grep -q -E "$security" "$file" || continue
  routed="$(mktemp)"
  sed -E "s#${security}#${mirror}#g" "$file" > "$routed"   # what followed (a `/`, a space) stays
  $as_root cp "$routed" "$file"
  rm -f "$routed"
  echo "ubuntu-security-via-mirror: $file now fetches security.ubuntu.com's pockets from $mirror"
done

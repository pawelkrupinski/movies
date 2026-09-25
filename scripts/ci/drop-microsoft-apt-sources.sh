#!/usr/bin/env bash
# Drop the GitHub-hosted Ubuntu runner's packages.microsoft.com apt sources before an
# `apt-get update`. Usage: scripts/ci/drop-microsoft-apt-sources.sh
#
# The runner image ships them for azure-cli, powershell, ...; nothing this repo installs
# comes from there. One 403 from that repository fails the whole `apt-get update` and the
# job with it (run 36104133560, a page-test shard, 2026-09-25: "packages.microsoft.com ...
# InRelease 403 Forbidden"). Dropping them leaves only the Ubuntu mirrors apt needs.
set -euo pipefail

sources="${APT_SOURCES_DIR:-/etc/apt/sources.list.d}"
grep -l 'packages\.microsoft\.com' "$sources"/*.list "$sources"/*.sources 2>/dev/null \
  | while read -r file; do sudo rm -f "$file"; done

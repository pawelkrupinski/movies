#!/usr/bin/env bash
# Install Playwright's browsers and their OS libraries on a GitHub-hosted Ubuntu runner.
# Usage: scripts/ci/install-playwright.sh <browser>...   (run from page-tests-playwright/)
#
# The runner image ships apt sources for packages.microsoft.com (azure-cli, powershell, ...),
# which nothing Playwright installs comes from. `--with-deps` runs `apt-get update`, and one
# 403 from that repository fails the whole update and the job with it (run 36104133560, a
# page-test shard, 2026-09-25: "packages.microsoft.com ... InRelease 403 Forbidden"). Dropping
# those sources first leaves only the Ubuntu mirrors apt actually needs.
set -euo pipefail

sources="${APT_SOURCES_DIR:-/etc/apt/sources.list.d}"
grep -l 'packages\.microsoft\.com' "$sources"/*.list "$sources"/*.sources 2>/dev/null \
  | while read -r file; do sudo rm -f "$file"; done

npx playwright install --with-deps "$@"

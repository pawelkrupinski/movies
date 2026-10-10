#!/usr/bin/env bash
# Install Playwright's browsers and their OS libraries on a GitHub-hosted Ubuntu runner.
# Usage: scripts/ci/install-playwright.sh <browser>...   (run from page-tests-playwright/)
#
# `--with-deps` runs `apt-get update`, so the runner's packages.microsoft.com sources go
# first (see drop-microsoft-apt-sources.sh).
#
# Each install is retried: Chrome comes from one 136 MB download off dl.google.com, and a
# stream dropped mid-way fails the whole page-test job ("curl: (92) HTTP/2 stream 1 was not
# closed cleanly", run 37999929093, 2026-10-09).
# Env: PLAYWRIGHT_INSTALL_ATTEMPTS (default 3), PLAYWRIGHT_RETRY_PAUSE_SECONDS (default 10).
# Tested by scripts/ci/install-playwright-test.sh.
set -euo pipefail

"$(dirname "${BASH_SOURCE[0]}")/drop-microsoft-apt-sources.sh"
attempts="${PLAYWRIGHT_INSTALL_ATTEMPTS:-3}"
for attempt in $(seq 1 "$attempts"); do
  npx playwright install --with-deps "$@" && exit 0
  echo "[playwright] installing $* failed (attempt $attempt of $attempts)" >&2
  [ "$attempt" -lt "$attempts" ] && sleep "${PLAYWRIGHT_RETRY_PAUSE_SECONDS:-10}"
done
echo "::error::could not install the Playwright browsers $* in $attempts attempts" >&2
exit 1

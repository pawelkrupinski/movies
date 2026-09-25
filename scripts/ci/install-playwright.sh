#!/usr/bin/env bash
# Install Playwright's browsers and their OS libraries on a GitHub-hosted Ubuntu runner.
# Usage: scripts/ci/install-playwright.sh <browser>...   (run from page-tests-playwright/)
#
# `--with-deps` runs `apt-get update`, so the runner's packages.microsoft.com sources go
# first (see drop-microsoft-apt-sources.sh).
set -euo pipefail

"$(dirname "${BASH_SOURCE[0]}")/drop-microsoft-apt-sources.sh"
npx playwright install --with-deps "$@"

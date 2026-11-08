#!/usr/bin/env bash
# Put the pinned actionlint into <dir> (the one install every workflow uses, through .github/actions/install-actionlint):
# its release tarball through fetch-verified.sh (a cache hit is re-verified, a miss downloaded with a real retry
# budget, the checksum always checked), then the binary out of it.
# Usage: scripts/ci/install-actionlint.sh <dir>   (env ACTIONLINT_VERSION, ACTIONLINT_SHA256)
# Tested by scripts/ci/install-actionlint-test.sh.
set -euo pipefail

dir="${1:?usage: install-actionlint.sh <dir>}"
: "${ACTIONLINT_VERSION:?}" "${ACTIONLINT_SHA256:?}"
archive="actionlint_${ACTIONLINT_VERSION}_linux_amd64.tar.gz"
"$(dirname "${BASH_SOURCE[0]}")/fetch-verified.sh" "$dir/$archive" \
  "https://github.com/rhysd/actionlint/releases/download/v$ACTIONLINT_VERSION/$archive" "$ACTIONLINT_SHA256"
tar -xzf "$dir/$archive" -C "$dir" actionlint

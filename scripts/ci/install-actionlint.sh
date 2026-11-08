#!/usr/bin/env bash
# Put the pinned actionlint, verified by its checksum, into <dir> (the one install every workflow uses, through
# .github/actions/install-actionlint).
# Usage: scripts/ci/install-actionlint.sh <dir>   (env ACTIONLINT_VERSION, ACTIONLINT_SHA256)
#
# <dir> is the action's actions/cache path: a tarball restored there whose checksum matches is used as it is, with no
# GitHub release download at all. Otherwise (a cold cache, or a restored file that does not match) it is downloaded
# with a real retry budget: on 2026-11-08 three lands' `check (rest)` failed (run 42466721258) on four 504s from
# GitHub's release CDN within ~7 s, the whole budget of the old `curl --retry 3`. Whatever the source, the tarball's
# sha256 is checked before the binary is extracted.
# Env: ACTIONLINT_CURL_RETRY (default 8), ACTIONLINT_CURL_RETRY_MAX_TIME (default 180 s).
# Tested by scripts/ci/install-actionlint-test.sh.
set -euo pipefail

dir="${1:?usage: install-actionlint.sh <dir>}"
: "${ACTIONLINT_VERSION:?}" "${ACTIONLINT_SHA256:?}"
archive="actionlint_${ACTIONLINT_VERSION}_linux_amd64.tar.gz"
tarball="$dir/$archive"
mkdir -p "$dir"

verified() { echo "$ACTIONLINT_SHA256  $tarball" | sha256sum -c --status - 2> /dev/null; }

if [ -f "$tarball" ] && verified; then
  echo "actionlint $ACTIONLINT_VERSION: restored from the cache, checksum verified"
else
  [ -f "$tarball" ] && echo "actionlint $ACTIONLINT_VERSION: the cached tarball fails its checksum -- downloading again" >&2
  rm -f "$tarball"
  curl -fsSL --retry "${ACTIONLINT_CURL_RETRY:-8}" --retry-all-errors \
    --retry-max-time "${ACTIONLINT_CURL_RETRY_MAX_TIME:-180}" --connect-timeout 20 --max-time 60 \
    -o "$tarball" "https://github.com/rhysd/actionlint/releases/download/v$ACTIONLINT_VERSION/$archive"
  if ! verified; then
    rm -f "$tarball"
    echo "::error::actionlint $ACTIONLINT_VERSION: the downloaded $archive does not match ACTIONLINT_SHA256" >&2
    exit 1
  fi
  echo "actionlint $ACTIONLINT_VERSION: downloaded, checksum verified"
fi
tar -xzf "$tarball" -C "$dir" actionlint

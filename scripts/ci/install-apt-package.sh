#!/usr/bin/env bash
#
# Install apt packages on a CI runner WITHOUT the unbounded wait a plain
# `apt-get update && apt-get install` has. A stalled mirror or a held dpkg lock made one
# recording's "Tunnel to prod Mongo" step sit on `apt-get` for 29 minutes before the job
# was cancelled (run 36922918266, scrapes (united-kingdom)), holding every enrichment leg
# behind it. Each attempt is now bounded and retried: a stall costs minutes, not the run.
#
# Usage:   scripts/ci/install-apt-package.sh <package>...
# Env:     APT_TIMEOUT_SECONDS  per apt-get call (default 120)
#          APT_ATTEMPTS         attempts before giving up (default 3)
# Tested by scripts/ci/install-apt-package-test.sh.
set -uo pipefail
[ "$#" -gt 0 ] || { echo "usage: $0 <package>..." >&2; exit 2; }
limit="${APT_TIMEOUT_SECONDS:-120}"
attempts="${APT_ATTEMPTS:-3}"
apt() { sudo timeout "$limit" apt-get -o DPkg::Lock::Timeout=60 -qq "$@"; }
for attempt in $(seq 1 "$attempts"); do
  if apt update && apt install -y "$@"; then
    exit 0
  fi
  echo "[apt] installing $* failed or ran past ${limit}s (attempt $attempt of $attempts)" >&2
  [ "$attempt" -lt "$attempts" ] && sleep "${APT_RETRY_PAUSE_SECONDS:-5}"
done
echo "::error::could not install $* within $attempts bounded attempts" >&2
exit 1

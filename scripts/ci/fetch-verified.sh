#!/usr/bin/env bash
# Make <file> the download of <url> whose sha256 is <sha256> -- the one fetch behind every pinned tool CI keeps in
# actions/cache (.github/actions/install-actionlint, .github/actions/setup-jdk).
# Usage: scripts/ci/fetch-verified.sh <file> <url> <sha256>
#
# <file> sits in an actions/cache path. A file there whose checksum matches is used as it is, with no download; a
# cache entry is never trusted as is, so one that does not match is dropped and downloaded again. A download retries
# for minutes, not seconds: on 2026-11-08 GitHub's release CDN answered 504 from ~08:54 to past 10:00 UTC, failing
# actionlint's ~7 s `curl --retry 3` (precommit run 42466721258) and setup-java's JDK download (Main #2306). Whatever
# the source, the checksum is checked before the caller uses the file, and a file that fails it is removed, so the
# cache never saves it.
# Env: FETCH_RETRY (default 8), FETCH_RETRY_MAX_TIME (default 180 s), FETCH_MAX_TIME (one attempt, default 300 s).
# Tested through its callers: scripts/ci/install-actionlint-test.sh, scripts/ci/jdk-pin-test.sh.
set -euo pipefail

file="${1:?usage: fetch-verified.sh <file> <url> <sha256>}"
url="${2:?usage: fetch-verified.sh <file> <url> <sha256>}"
sha256="${3:?usage: fetch-verified.sh <file> <url> <sha256>}"
name="$(basename "$file")"
mkdir -p "$(dirname "$file")"

verified() { echo "$sha256  $file" | sha256sum -c --status - 2> /dev/null; }

if [ -f "$file" ] && verified; then
  echo "$name: restored from the cache, checksum verified"
  exit 0
fi
if [ -f "$file" ]; then echo "$name: the cached file fails its checksum -- downloading again" >&2; fi
rm -f "$file"
curl -fsSL --retry "${FETCH_RETRY:-8}" --retry-all-errors --retry-max-time "${FETCH_RETRY_MAX_TIME:-180}" \
  --connect-timeout 20 --max-time "${FETCH_MAX_TIME:-300}" -o "$file" "$url"
if ! verified; then
  rm -f "$file"
  echo "::error::$name: the download of $url does not match its pinned sha256 $sha256" >&2
  exit 1
fi
echo "$name: downloaded, checksum verified"

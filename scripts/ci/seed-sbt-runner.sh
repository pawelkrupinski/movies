#!/usr/bin/env bash
# Put the pinned sbt runner into the runner's tool cache, where sbt/setup-sbt looks first, so that setup-sbt itself
# downloads nothing. Every `uses: sbt/setup-sbt@` step comes right after this one (SetupSbtRunnerPinSpec).
# Usage: scripts/ci/seed-sbt-runner.sh [version] [sha256]   (defaults: the pin below, the job image's runner)
#
# setup-sbt's own download is a single `curl -sL` with no retry. On a GitHub-hosted runner whose DNS blinked, it failed a
# convergence leg (curl exit 6 = could not resolve host; Identity model convergence #1195, 2026-11-16). This downloads
# through fetch-verified.sh instead, which retries for minutes and checks the release's sha256. A download that still
# fails is named by its curl exit code, so the next occurrence says what broke.
# A zip already at $RUNNER_TEMP/_sbt-seed whose checksum matches (restored from actions/cache by
# .github/actions/seed-sbt-runner) is used with no download at all.
# A runner whose image already holds the runner (`SBT_RUNNER_URL` in the job image) does nothing here.
# Env: RUNNER_TOOL_CACHE, RUNNER_TEMP (set by the runner); FETCH_* as fetch-verified.sh.
# Test: scripts/ci/seed-sbt-runner-test.sh.
set -euo pipefail

version="${1:-2.0.10}"
sha256="${2:-6ff28e1dea38d1de48ea3d4e903f0380b85b19f945a103e30ec4298717c90cd7}"
toolpath="${RUNNER_TOOL_CACHE:?RUNNER_TOOL_CACHE unset}/sbt/$version"
launcher="$toolpath/sbt/bin/sbt"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

if [ -f "$launcher" ]; then
  echo "sbt runner $version: already in the tool cache ($toolpath)"
  exit 0
fi

zip="${RUNNER_TEMP:?RUNNER_TEMP unset}/_sbt-seed/sbt-$version.zip"
url="https://github.com/sbt/sbt/releases/download/v$version/sbt-$version.zip"
rc=0
"$here/fetch-verified.sh" "$zip" "$url" "$sha256" || rc=$?
if [ "$rc" != 0 ]; then
  case "$rc" in
    6) why="could not resolve the host (DNS)" ;;
    7) why="could not connect" ;;
    28) why="timed out" ;;
    22) why="the server answered an HTTP error" ;;
    56) why="the connection died while receiving" ;;
    35) why="the TLS handshake failed" ;;
    *) why="exit $rc" ;;
  esac
  echo "::error::sbt runner $version: the download of $url failed after its retries: curl exit $rc, $why" >&2
  exit "$rc"
fi
# the verified zip, for .github/actions/seed-sbt-runner to keep in actions/cache
if [ -n "${GITHUB_OUTPUT:-}" ]; then echo "zip=verified" >> "$GITHUB_OUTPUT"; fi
mkdir -p "$toolpath"
unzip -q -o "$zip" -d "$toolpath"
if [ ! -f "$launcher" ]; then
  echo "::error::sbt runner $version: $zip holds no runner launcher (sbt/bin/sbt)" >&2
  exit 1
fi
echo "sbt runner $version: seeded into the tool cache ($toolpath)"

#!/usr/bin/env bash
# Purpose: Delete coursier's negative cache records so a retried fetch asks the repository again
# Delete coursier's negative records -- the `.<file>.error` markers its file cache writes when a repository answers 404
# for an artifact whose metadata it already holds (coursier-cache's Downloader: `remoteKeepErrors`, honoured by
# `checkErrFile` without asking the repository again) -- from every coursier cache this machine has, keeping the files.
# A 404 from Maven Central can be wrong for a minute (Main #2678), so a marker must neither outlive the run that got it
# nor reach a saved dependency cache: scripts/ci/sbt-guard/sbt runs this before retrying a failed download, and
# scripts/ci/sbt-deps-resolve.sh before ci.yml's `deps` job saves the cache. Prints how many it deleted.
# Usage: scripts/ci/coursier-forget-errors.sh   (COURSIER_CACHE, or ~/.cache/coursier and ~/Library/Caches/Coursier)
# Test: scripts/ci/sbt-guard-test.sh, scripts/ci/sbt-deps-resolve-test.sh.
set -euo pipefail

deleted=0
for dir in "${COURSIER_CACHE:-}" "$HOME/.cache/coursier" "$HOME/Library/Caches/Coursier"; do
  [ -n "$dir" ] && [ -d "$dir" ] || continue
  n=$(find "$dir" -type f -name '.*.error' -print -delete | wc -l)
  deleted=$((deleted + n))
done
echo "coursier-forget-errors: deleted $deleted negative record(s)"

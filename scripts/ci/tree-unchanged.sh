#!/usr/bin/env bash
#
# A test run must leave every CHECKED-IN file exactly as it found it.
#
# The snapshot guards write what they would have expected — CatalogSeedSpec rewrites both
# apps' catalog-seed.json, FilmScheduleEndToEndSpec and PageSnapshotSpec write a missing
# expected-schedules.txt / read-model-snapshot.json / expected-*.html — and are meant to fail
# while doing it. This holds the whole class to that: if any spec (or a generator it calls)
# rewrote a committed file, the run that did it fails here with the diff to commit, even when
# the spec itself stayed green. Untracked output (reports, caches) is not its business.
#
# Usage: scripts/ci/tree-unchanged.sh "<what just ran>"
set -euo pipefail

what=${1:?usage: tree-unchanged.sh "<what just ran>"}
drift=$(git status --porcelain --untracked-files=no)
if [ -n "$drift" ]; then
    echo "::error::$what rewrote checked-in files — a snapshot or generated artefact is stale. Regenerate it locally (see .claude/skills/regenerate-snapshots) and commit:"
    echo "$drift"
    git --no-pager diff --stat
    git --no-pager diff | head -200
    exit 1
fi
echo "$what left every checked-in file unchanged."

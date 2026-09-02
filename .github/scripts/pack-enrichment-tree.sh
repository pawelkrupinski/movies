#!/usr/bin/env bash
#
# Tar the enrichment tree a convergence leg recorded, and refuse to publish an
# archive that lost the remembered-answer cache on the way in.
#
#   pack-enrichment-tree.sh <tree dir> <archive path>
#
# A shell FILE rather than an inline `run:` block so the guard below can be run
# against a fabricated tree by `EnrichmentTreePackingSpec` — the same shape
# `.github/actions/changed-paths/matches.sh` has, for the same reason: a rule
# that only exists inside a workflow can only be checked by pushing.
set -uo pipefail

DIR="${1:?usage: pack-enrichment-tree.sh <tree dir> <archive path>}"
ARCHIVE="${2:?usage: pack-enrichment-tree.sh <tree dir> <archive path>}"

# Nothing to pack is a normal outcome, not a failure: the first run on a new
# country recorded nothing yet, and a leg that died before it enriched anything
# has no capture to hand on.
if [ ! -d "$DIR" ]; then
    echo "nothing recorded — no $DIR"
    exit 0
fi

echo "recorded fixture files: $(find "$DIR" -type f -not -path '*/.enrichment-cache/*' | wc -l | tr -d '[:space:]')"
remembered=$( { find "$DIR/.enrichment-cache" -name '*.entry' 2>/dev/null || true; } | wc -l | tr -d '[:space:]' )
echo "remembered enrichment answers: $remembered"

mkdir -p "$(dirname "$ARCHIVE")"

# NOT `set -e` around this tar, and the exit code is graded rather than
# tested for zero.
#
# The publish step runs on `always()`, so its most valuable case is the leg that
# just ran out of time — and a step killed by `timeout-minutes` does not
# take the JVM with it instantly. The runner only reaps orphans in its
# post-job phase, well after this, so `RecordingHttpFetch` is still
# writing responses into the very tree being read. GNU tar notices, prints
# "file changed as we read it", and exits 1 — a WARNING status, with a
# complete and perfectly valid archive on disk. Under `set -e` that
# failed the step and threw away the whole capture, which is precisely
# the "a timeout that discards its own progress cannot converge" trap the
# publish exists to close. Germany's first full leg in a week lost its
# entire corpus capture to it.
#
# 2 and above is a real tar failure (unwritable target, corrupt stream)
# and still fails.
tar -czf "$ARCHIVE" "$DIR"
packed=$?
if [ "$packed" -gt 1 ]; then
    echo "::error::tar failed with status $packed"
    exit "$packed"
fi
if [ "$packed" -eq 1 ]; then
    echo "tar reported files changing under it — the leg's JVM is still recording; archive kept"
fi
du -h "$ARCHIVE"

# The cache is dot-prefixed and lives INSIDE the tree, so `tar` carries it
# along with the recorded responses and one asset restores both. Asserted
# rather than assumed: if a future change to this tar drops hidden paths, the
# loss is invisible — every leg simply gets slower and still passes.
# `grep -c`, which reads to EOF, NOT `grep -q`, which doesn't.
#
# `tar -tzf … | grep -q` exits the moment it matches and closes the pipe; GNU
# tar is still streaming, takes SIGPIPE and returns 141, and under
# `set -euo pipefail` the PIPELINE is then a failure even though the pattern
# was found. The `!` turned that into "the cache is missing", so this guard
# failed all three legs on a tarball that contained the cache perfectly — the
# published asset had 3,745 cache paths in it. Reading the listing to the end
# removes the race entirely, and the count is worth printing anyway.
#
# (BSD tar on macOS absorbs the SIGPIPE, so this could not be reproduced
# locally — it needs a Linux runner. Verified instead by inspecting the asset
# the same run published.)
cached=$(tar -tzf "$ARCHIVE" | grep -c '/\.enrichment-cache/.*\.entry' || true)
echo "remembered answers inside the archive: $cached"

# Compared against what the TREE holds, not against the mere existence of the
# cache directory. An empty `.enrichment-cache/` is the normal state of a leg
# that recorded nothing — a country on its first run, or one whose suite failed
# before enrichment — and `-d $DIR/.enrichment-cache` alone called that "the
# cache is missing from the tarball" and failed the step. Spain's first
# convergence leg died that way on 2026-09-02: it had already failed for want of
# a corpus, and then failed a second time for losing a cache that never existed,
# which is a red herring in front of the real one.
if [ "$remembered" -gt 0 ] && [ "$cached" -eq 0 ]; then
    echo "::error::the enrichment cache exists on disk but is NOT in the tarball"
    exit 1
fi

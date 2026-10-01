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

# Compressed on every core. Single-threaded gzip was 75 s of the US recording's critical
# path (117k files, 586 MB packed — run 36909637796); the runner image ships pigz, whose
# output is the same gzip every reader of the asset unpacks (`tar -xzf`). Plain gzip
# where pigz is absent (a developer's machine), so the archive never depends on it.
compress=gzip
command -v pigz >/dev/null 2>&1 && compress=pigz

# The listing tar prints as it streams (`-v`, to stderr when the archive is stdout — GNU
# and BSD tar alike) is what the cache guard below counts: the paths that went INTO the
# archive. Reading the finished archive back instead was a second full gunzip of it, 16 s
# of the same US publish, to recover a list tar had already printed.
listing=$(mktemp)
trap 'rm -f "$listing"' EXIT

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
# and still fails — as does any failure of the compressor.
tar -cvf - "$DIR" 2>"$listing" | "$compress" > "$ARCHIVE"
statuses=("${PIPESTATUS[@]}")
packed=${statuses[0]}
compressed=${statuses[1]}
# tar's own messages share the listing; they are not paths, and belong in the log.
grep '^[a-z]*tar: ' "$listing" || true
if [ "$packed" -gt 1 ]; then
    echo "::error::tar failed with status $packed"
    exit "$packed"
fi
if [ "$compressed" -ne 0 ]; then
    echo "::error::$compress failed with status $compressed"
    exit "$compressed"
fi
if [ "$packed" -eq 1 ]; then
    echo "tar reported files changing under it — the leg's JVM is still recording; archive kept"
fi
du -h "$ARCHIVE"

# The cache is dot-prefixed and lives INSIDE the tree, so `tar` carries it
# along with the recorded responses and one asset restores both. Asserted
# rather than assumed: if a future change to this tar drops hidden paths, the
# loss is invisible — every leg simply gets slower and still passes.
# Counted off tar's own listing of what it archived (BSD tar prefixes each path
# with `a `, which the pattern does not anchor on).
cached=$(grep -c '/\.enrichment-cache/.*\.entry' "$listing" || true)
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

#!/usr/bin/env bash
# Keep an origin's PREVIOUS recording in a tree this recording is about to publish, when the origin was down for everyone
# (recording_health.py's `origin-down`: a 5xx both direct and through production's proxy).
#
#   keep-previous-origins.sh <archive dir> <tree> <host>...
#
# The owner, 2026-11-14, deciding Record scrape fixtures #186: www.kinomuza.pl answered 503 to every route all night, and
# the tree the leg publishes held only what the 5-day prune (`EnrichmentFreshness.Ttl`) had left of its last recording --
# 72 of 89 responses, and none at all once an outage outlasts the prune. Each <host>'s files are put back from the
# previous recording's archive (the one setup restored the tree from: restore-enrichment-tree.sh leaves it in
# <stage>.archive), WITHOUT replacing a file this recording wrote, so the published tree carries the venue's last good
# answers rather than an empty one.
#
# The archive is a zstd or gzip tar (by its magic, as unpack-fixture-archive.sh reads it) whose paths are the tree's own
# (`<tree>/<host>/...`, pack-enrichment-tree.sh). Nothing to restore (no archive, no such host in it) is not an error:
# a first recording has no previous one. Exit 0 then, and 1 only when an archive cannot be read.
set -uo pipefail

archives="${1:?usage: keep-previous-origins.sh <archive dir> <tree> <host>...}"
tree="${2:?usage: keep-previous-origins.sh <archive dir> <tree> <host>...}"
shift 2
[ "$#" -gt 0 ] || exit 0
archive=$(find "$archives" -maxdepth 1 -type f 2>/dev/null | head -1)
if [ -z "$archive" ]; then
    echo "keep-previous-origins: no previous recording in $archives -- nothing to keep for $*"
    exit 0
fi
magic=$(head -c 4 "$archive" | od -An -tx1 | tr -d ' \n')
case "$magic" in
    28b52ffd) inflate=(zstd -q -dc) ;;
    1f8b*)    inflate=(gzip -dc) ;;
    *)        echo "::error::keep-previous-origins: $archive is neither zstd nor gzip (magic ${magic:-none})" >&2; exit 1 ;;
esac
# GNU tar skips a file already on disk with --skip-old-files and takes a member pattern with --wildcards; bsdtar's -k
# keeps an existing file silently, and its member patterns are globs already. Run from the directory the archive's paths
# are relative to (the workspace), <tree> given as those paths name it.
if tar --version 2>/dev/null | grep -q GNU; then keep=(--skip-old-files --wildcards); else keep=(-k); fi
tree=${tree%/}
for host in "$@"; do
    before=$(find "$tree/$host" -type f 2>/dev/null | wc -l | tr -d ' ')
    "${inflate[@]}" "$archive" | tar -xf - "${keep[@]}" "$tree/$host/*" 2>/dev/null
    after=$(find "$tree/$host" -type f 2>/dev/null | wc -l | tr -d ' ')
    echo "keep-previous-origins: $host down for everyone -- kept $((after - before)) response(s) of its previous recording ($after in the tree)"
done
exit 0

#!/usr/bin/env bash
#
# Unpack a recorded-fixture archive, whichever compressor packed it.
#
#   unpack-fixture-archive.sh <archive> [directory, default .]
#
# The enrichment tree is packed with zstd (`pack-enrichment-tree.sh`), but the release
# still holds gzip trees pinned before that, and the scrape corpus is still a gzip — so
# every reader decides by the archive's MAGIC, never by its name or a `tar -z` it assumed.
# gzip inflates through pigz where the runner has it (about twice gzip's pace on the US
# tree, run 36909637796); zstd decompresses on its own thread beside tar either way.
set -euo pipefail

ARCHIVE="${1:?usage: unpack-fixture-archive.sh <archive> [directory]}"
INTO="${2:-.}"

magic=$(head -c 4 "$ARCHIVE" | od -An -tx1 | tr -d ' \n')
case "$magic" in
    28b52ffd) inflate=(zstd -q -dc) ;;
    1f8b*)    if command -v pigz >/dev/null 2>&1; then inflate=(pigz -dc); else inflate=(gzip -dc); fi ;;
    *)        echo "::error::$ARCHIVE is neither zstd nor gzip (magic ${magic:-none})" >&2; exit 1 ;;
esac

mkdir -p "$INTO"
"${inflate[@]}" "$ARCHIVE" | tar -xf - -C "$INTO"

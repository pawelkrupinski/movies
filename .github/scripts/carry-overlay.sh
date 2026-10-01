#!/usr/bin/env bash
# Carry an earlier recording's identity overlay into a leg replaying a NEW recording, run from the
# repository root: unpack only the answers the new tree lacks — never replacing a file it holds, and
# leaving the remembered verdicts (`.enrichment-cache/`) behind, so a past 429 or timeout is asked
# again rather than replayed — and write the files it added to <files-out>, which the overlay publish
# packs with what the leg records next (convergence-overlay-publish).
#
#   carry-overlay.sh <overlay.tar.gz> <files-out>
set -uo pipefail
archive=$1
added=$2
tar -tzf "$archive" | grep -v '/$' | grep -v '/\.enrichment-cache/' \
    | while IFS= read -r f; do [ -e "$f" ] || printf '%s\n' "$f"; done > "$added"
if [ -s "$added" ]; then tar -xzf "$archive" -T "$added"; fi
echo "carried $(wc -l < "$added" | tr -d ' ') answer(s) the tree lacks forward from $(basename "$archive")"

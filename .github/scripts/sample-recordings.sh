#!/usr/bin/env bash
# Hand what a RECORDING's sample row recorded to the convergence row that publishes the tree.
#
#   sample-recordings.sh pack  <tree dir> <stamp> <archive>   # the files the sample wrote
#   sample-recordings.sh merge <archive>                       # laid into this workspace's tree
#
# WHY. A recording's sample used to run in its convergence row, ahead of the suite, and record
# into the tree the suite then extended: ~1 minute of the longest leg's critical path (the United
# States', run 37105119296) for a step that gates nothing in a recording. In a row of its own it
# runs beside the boot, over the same corpus and the same restored tree, and its recordings reach
# the one publish through this: `pack` takes every file under the tree newer than a stamp touched
# just before the sample ran (an unpacked file keeps its archived mtime, so only what the sample
# wrote is newer), and `merge` lays them into the convergence row's tree before it is packed.
#
# Merge rules — both rows record real answers, so the question is only which to keep:
#   - a file the convergence row lacks is taken (a request only the sample made, or a stale entry
#     the convergence row expired and never re-asked);
#   - a file both have is the NEWER one's (each row's mtime is when it recorded the answer; an
#     untouched file in the convergence row still carries its archived, older mtime);
#   - the identity lookups' marker is the UNION of both, exactly as `IdentityLookupSweep
#     .markRecorded` keeps the names already there when a second leg records into one tree.
set -euo pipefail

Marker=".identity-lookups-v3"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

case "${1:-}" in
  pack)
    tree="${2:?tree dir}"; stamp="${3:?stamp}"; archive="${4:?archive}"
    mkdir -p "$(dirname "$archive")"
    list=$(mktemp)
    trap 'rm -f "$list"' EXIT
    if [ -d "$tree" ]; then find "$tree" -type f -newer "$stamp" > "$list"; fi
    echo "the sample recorded $(wc -l < "$list" | tr -d ' ') file(s) under $tree"
    tar -cf - -T "$list" | zstd -q -T0 -3 -c > "$archive"
    ;;
  merge)
    archive="${2:?archive}"
    work=$(mktemp -d)
    trap 'rm -rf "$work"' EXIT
    "$here/unpack-fixture-archive.sh" "$archive" "$work"
    taken=0; kept=0
    while IFS= read -r -d '' file; do
      dest="${file#"$work"/}"
      mkdir -p "$(dirname "$dest")"
      if [ "$(basename "$file")" = "$Marker" ] && [ -f "$dest" ]; then
        sort -u "$dest" "$file" | grep -v '^$' > "$dest.merged" || true
        mv "$dest.merged" "$dest"
      elif [ ! -e "$dest" ] || [ "$file" -nt "$dest" ]; then
        cp -p "$file" "$dest"; taken=$((taken + 1))
      else
        kept=$((kept + 1))
      fi
    done < <(find "$work" -type f -print0)
    echo "merged the sample's recordings: $taken taken, $kept already newer here"
    ;;
  *)
    echo "usage: $0 pack <tree dir> <stamp> <archive> | merge <archive>" >&2; exit 64 ;;
esac

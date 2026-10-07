#!/usr/bin/env bash
# The release side of a convergence leg's FILL: what a hermetic leg's tree lacked, fetched by the next
# leg and published beside the pinned pair, so the gap closes run over run instead of waiting for the
# next recording (docs/design/convergence-fixture-fill.md).
#
#   convergence-fill.sh fills  <code> <corpus run>               # the pair's fill assets, oldest first, one line
#   convergence-fill.sh unpack <stage dir> [fill asset...]       # each laid over a staged tree
#   convergence-fill.sh gaps   <code> <corpus run> <out file>    # the newest refetch list; prints its line count
#   convergence-fill.sh pack   <fill dir> <archive>              # what a fill fetched; no archive when nothing
#
# Reads FIXTURE_RELEASE_TAG and GH_TOKEN from the environment.
#
# NAMES, NEVER A REPLACEMENT. A fill is `fill-<code>-<corpus run>-<run id>.tar.zst` and a refetch list
# `refetch-<code>-<corpus run>-<run id>.tsv`: each publish adds an asset under a name nobody else writes,
# so no reader ever meets the moment `--clobber` deletes an asset before re-uploading it, and five
# countries publishing side by side never touch each other's names. Readers take every fill of the pair
# (`fills`) and the NEWEST list (`gaps`); the pin step prunes both with the pair they belong to.
#
# Best effort throughout: a fill a leg cannot read is a fill it replays without, never a failed leg.
set -uo pipefail

TAG="${FIXTURE_RELEASE_TAG:?FIXTURE_RELEASE_TAG names the rolling release}"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Every asset name in the release, one per line; nothing (and a warning) when it cannot be read.
assets() {
  gh release view "$TAG" --json assets --jq '.assets[].name' 2>/dev/null ||
    echo "::warning::could not list release $TAG — no fills this time" >&2
}

# The names matching ^<prefix>-<code>-<corpus>-<run id>.<ext>$, by run id ascending.
named() {
  local prefix="$1" code="$2" corpus="$3" ext="$4"
  assets | grep -E "^$prefix-$code-$corpus-[0-9]+\\.$ext\$" | sort -t- -k4,4n
}

case "${1:-}" in
  fills)
    code="${2:?code}"; corpus="${3:?corpus run}"
    named fill "$code" "$corpus" 'tar\.zst' | paste -sd' ' -
    ;;
  unpack)
    stage="${2:?stage dir}"; shift 2
    [ "$#" -gt 0 ] || exit 0
    downloads="$stage.fills"
    mkdir -p "$downloads" "$stage"
    for fill in "$@"; do
      if gh release download "$TAG" --pattern "$fill" --dir "$downloads" --clobber 2>/dev/null &&
         "$here/unpack-fixture-archive.sh" "$downloads/$fill" "$stage"; then
        echo "laid fill $fill over the tree"
      else
        echo "::warning::fill $fill could not be restored — this leg replays without it"
      fi
    done
    ;;
  gaps)
    code="${2:?code}"; corpus="${3:?corpus run}"; out="${4:?out file}"
    newest=$(named refetch "$code" "$corpus" tsv | tail -1)
    : > "$out"
    if [ -n "$newest" ] && gh release download "$TAG" --pattern "$newest" --output "$out" --clobber 2>/dev/null; then
      echo "::notice::$newest lists $(grep -c . "$out" || true) fetchable gap(s)" >&2
    fi
    grep -c . "$out" || true
    ;;
  pack)
    dir="${2:?fill dir}"; archive="${3:?archive}"
    files=$( { find "$dir" -type f 2>/dev/null || true; } | wc -l | tr -d ' ')
    if [ "$files" -eq 0 ]; then echo "the fill fetched nothing — no archive"; exit 0; fi
    mkdir -p "$(dirname "$archive")"
    tar -C "$dir" -cf - . | zstd -q -T0 -3 -c > "$archive" || exit 1
    echo "packed $files fetched file(s) into $archive ($(du -h "$archive" | cut -f1))"
    ;;
  *)
    echo "usage: $0 fills <code> <corpus> | unpack <stage> [asset...] | gaps <code> <corpus> <out> | pack <dir> <archive>" >&2
    exit 64 ;;
esac

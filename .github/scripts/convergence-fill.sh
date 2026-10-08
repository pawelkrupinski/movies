#!/usr/bin/env bash
# The release side of a convergence leg's FILL: what a hermetic leg's tree lacked, fetched by the next
# leg's rows before their suites and published beside the pinned pair, so the gap closes run over run instead of waiting for the
# next recording (docs/design/convergence-fixture-fill.md).
#
#   convergence-fill.sh fills  <code> <corpus run>               # the pair's fill assets, oldest first, one line
#   convergence-fill.sh unpack <stage dir> [fill asset...]       # each laid over a staged tree
#   convergence-fill.sh gaps   <code> <corpus run> <out file>    # the newest refetch list; prints its line count
#   convergence-fill.sh pack   <fill dir> <archive>              # what a fill fetched; no archive when nothing
#
# Reads FIXTURE_RELEASE_TAG and GH_TOKEN from the environment.
#
# NAMES, NEVER A REPLACEMENT. A fill is `fill-<code>-<corpus run>-<run id>[-<row>].tar.zst` — a leg row's
# pre-suite fill carries its phase (`-convergence`, `-order-independence`, `-sample`), the manual
# `Convergence fill` workflow's none — and a refetch list `refetch-<code>-<corpus run>-<run id>.tsv`: each
# publish adds an asset under a name nobody else writes, so no reader ever meets the moment `--clobber`
# deletes an asset before re-uploading it, and five countries' rows publishing side by side never touch
# each other's names. Readers take every fill of the pair
# (`fills`) and the NEWEST list (`gaps`); the pin step prunes both with the pair they belong to.
#
# A fill a leg cannot DOWNLOAD is a fill it replays without, never a failed leg. A release it cannot LIST,
# asked three times, fails `fills` and `gaps` loudly instead: a failed listing is not an empty release, and read as "no fills"
# it would let the leg decide its verdict on — and pin into its bisect's pair — a tree without the fills
# its pair has. Only a listing that succeeds and names none means no fills.
set -uo pipefail

TAG="${FIXTURE_RELEASE_TAG:?FIXTURE_RELEASE_TAG names the rolling release}"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Every asset name in the release, one per line; an error and a non-zero status when it cannot be listed.
# Asked up to three times, the wait doubling from CONVERGENCE_FILL_LIST_BACKOFF seconds (5): one GitHub 5xx is
# not a release that cannot be listed, and failing the leg on it re-runs a whole convergence row.
assets() {
  local names attempt=1 wait="${CONVERGENCE_FILL_LIST_BACKOFF:-5}" attempts=3
  until names=$(gh release view "$TAG" --json assets --jq '.assets[].name'); do
    if [ "$attempt" -ge "$attempts" ]; then
      echo "::error::could not list release $TAG after $attempts attempts — its fills are unknown, not none" >&2
      return 1
    fi
    echo "::warning::listing release $TAG failed (attempt $attempt of $attempts); asking again in ${wait}s" >&2
    sleep "$wait"
    attempt=$((attempt + 1)); wait=$((wait * 2))
  done
  [ -z "$names" ] || printf '%s\n' "$names"
}

# The names matching ^<prefix>-<code>-<corpus>-<run id>[-<row>].<ext>$, by run id ascending — one run's
# rows then by name, in the C locale, so every reader lays them over in the same order.
named() {
  local prefix="$1" code="$2" corpus="$3" ext="$4" names
  names=$(assets) || return 1
  printf '%s\n' "$names" | { grep -E "^$prefix-$code-$corpus-[0-9]+(-[a-z][a-z-]*)?\\.$ext\$" || true; } | LC_ALL=C sort -t- -k4,4n
}

case "${1:-}" in
  fills)
    code="${2:?code}"; corpus="${3:?corpus run}"
    names=$(named fill "$code" "$corpus" 'tar\.zst') || exit 1
    printf '%s\n' "$names" | paste -sd' ' -
    ;;
  unpack)
    stage="${2:?stage dir}"; shift 2
    [ "$#" -gt 0 ] || exit 0
    downloads="$stage.fills"
    mkdir -p "$downloads" "$stage"
    for fill in "$@"; do
      # allow-silenced: a fill that cannot be read is replayed without, and the warning below says so.
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
    newest=$(named refetch "$code" "$corpus" tsv) || exit 1
    newest=$(printf '%s\n' "$newest" | tail -1)
    : > "$out"
    # allow-silenced: a list that cannot be read is no gaps this time — the next leg's fill reads it again.
    if [ -n "$newest" ] && gh release download "$TAG" --pattern "$newest" --output "$out" --clobber 2>/dev/null; then
      echo "::notice::$newest lists $(grep -c "$(printf '\t')" "$out" || true) fetchable gap(s)" >&2
    fi
    grep -c "$(printf '\t')" "$out" || true
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

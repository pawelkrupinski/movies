#!/usr/bin/env bash
# The release side of a convergence leg's FILL: what a hermetic leg's tree lacked, fetched by the next
# leg's convergence row before its suite and published beside the pinned pair, so the gap closes run over run instead
# of waiting for the next recording (docs/design/convergence-fixture-fill.md).
#
#   convergence-fill.sh fills  <code> <corpus run>               # the pair's fill assets, oldest first, one line
#   convergence-fill.sh unpack <stage dir> [fill asset...]       # each laid over a staged tree
#   convergence-fill.sh gaps   <code> <corpus run> <out file>    # the newest refetch list, less what was refused lately; prints its line count
#   convergence-fill.sh pack   <fill dir> <archive>              # what a fill fetched; no archive when nothing
#
# Reads FIXTURE_RELEASE_TAG and GH_TOKEN from the environment.
#
# NAMES, NEVER A REPLACEMENT. A fill is `fill-<code>-<corpus run>-<run id>-<run attempt>[-<row>].tar.zst` — a leg's
# pre-suite fill carries its row (`-convergence`), the manual `Convergence fill` workflow's none — a refetch list
# `refetch-<code>-<corpus run>-<run id>-<run attempt>.tsv`, and a fill's refused gaps
# `refused-<code>-<corpus run>-<run id>-<run attempt>.tsv`: each publish adds an asset under a name nobody else writes —
# a re-run of a failed job included, whose attempt differs — so no reader ever meets the moment `--clobber` deletes an
# asset before re-uploading it, and five countries' rows publishing side by side never touch each other's names.
# (Names from before the attempt was part of them, `<run id>[-<row>]`, still read, as attempt 0.) Readers take every
# fill of the pair (`fills`), the NEWEST list (`gaps`) and the newest few refused lists; the pin step prunes them all
# with the pair they belong to.
#
# A release it cannot LIST, asked three times, fails `fills` and `gaps` loudly: a failed listing is not an empty
# release, and read as "no fills" it would let the leg decide its verdict on — and pin into its bisect's pair — a tree
# without the fills its pair has. Only a listing that succeeds and names none means no fills. A fill it cannot DOWNLOAD
# or unpack, asked three times, fails `unpack` the same way, for the same reason: the leg's pair names it, so a leg
# replaying without it would decide its verdict on — and hand its bisect — a pair it never replayed.
set -uo pipefail

TAG="${FIXTURE_RELEASE_TAG:?FIXTURE_RELEASE_TAG names the rolling release}"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# How many of the newest refused lists a fill skips the gaps of: a gap an origin refused is asked again only once
# this many later fills have published lists without it — never every run through paid egress.
REFUSED_KEPT="${CONVERGENCE_FILL_REFUSED_KEPT:-4}"

# Asked up to three times, the wait doubling from CONVERGENCE_FILL_LIST_BACKOFF seconds (5): one GitHub 5xx is not a
# release that cannot be read, and failing the leg on it re-runs a whole convergence row.
attempts=3
backoff() { sleep "$(( ${CONVERGENCE_FILL_LIST_BACKOFF:-5} << ($1 - 1) ))"; }

# Every asset name in the release, one per line; an error and a non-zero status when it cannot be listed.
assets() {
  local names attempt=1
  until names=$(gh release view "$TAG" --json assets --jq '.assets[].name'); do
    if [ "$attempt" -ge "$attempts" ]; then
      echo "::error::could not list release $TAG after $attempts attempts — its fills are unknown, not none" >&2
      return 1
    fi
    echo "::warning::listing release $TAG failed (attempt $attempt of $attempts); asking again" >&2
    backoff "$attempt"
    attempt=$((attempt + 1))
  done
  [ -z "$names" ] || printf '%s\n' "$names"
}

# The names matching ^<prefix>-<code>-<corpus>-<run id>[-<attempt>][-<row>].<ext>$, by run id and then attempt
# ascending — one attempt's rows then by name, in the C locale, so every reader lays them over in the same order.
named() {
  local prefix="$1" code="$2" corpus="$3" ext="$4" names
  names=$(assets) || return 1
  printf '%s\n' "$names" | { grep -E "^$prefix-$code-$corpus-[0-9]+(-[0-9]+)?(-[a-z][a-z-]*)?\\.$ext\$" || true; } |
    LC_ALL=C sort -t- -k4,4n -k5,5n
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
    status=0
    for fill in "$@"; do
      attempt=1
      # allow-silenced: gh's own message is noise beside the error below, which names the fill.
      until gh release download "$TAG" --pattern "$fill" --dir "$downloads" --clobber 2>/dev/null &&
            "$here/unpack-fixture-archive.sh" "$downloads/$fill" "$stage"; do
        if [ "$attempt" -ge "$attempts" ]; then
          echo "::error::fill $fill could not be restored after $attempts attempts — the pair names it, so this leg will not replay without it"
          status=1
          continue 2
        fi
        backoff "$attempt"
        attempt=$((attempt + 1))
      done
      echo "laid fill $fill over the tree"
    done
    exit "$status"
    ;;
  gaps)
    code="${2:?code}"; corpus="${3:?corpus run}"; out="${4:?out file}"
    newest=$(named refetch "$code" "$corpus" tsv) || exit 1
    newest=$(printf '%s\n' "$newest" | tail -1)
    refused=$(named refused "$code" "$corpus" tsv) || exit 1
    refused=$(printf '%s\n' "$refused" | tail -n "$REFUSED_KEPT")
    : > "$out"
    # allow-silenced: a list that cannot be read is no gaps this time — the next leg's fill reads it again.
    if [ -n "$newest" ] && gh release download "$TAG" --pattern "$newest" --output "$out" --clobber 2>/dev/null; then
      echo "::notice::$newest lists $(grep -c "$(printf '\t')" "$out" || true) fetchable gap(s)" >&2
      skip="$out.refused"
      : > "$skip"
      for list in $refused; do
        # allow-silenced: a refused list that cannot be read only means its gaps are asked again.
        gh release download "$TAG" --pattern "$list" --output "$skip.part" --clobber 2>/dev/null && cat "$skip.part" >> "$skip"
      done
      if [ -s "$skip" ]; then
        before=$(grep -c "$(printf '\t')" "$out" || true)
        awk -F'\t' 'NR == FNR { if (NF > 1) refused[$1] = 1; next } !($1 in refused)' "$skip" "$out" > "$out.kept" && mv "$out.kept" "$out"
        echo "::notice::skipping $((before - $(grep -c "$(printf '\t')" "$out" || true))) gap(s) an origin refused in the last $REFUSED_KEPT fill(s)" >&2
      fi
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

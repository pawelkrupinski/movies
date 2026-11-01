#!/usr/bin/env bash
# The release side of a convergence leg's FILL: what a hermetic leg's tree lacked, fetched by the next
# leg's convergence row before its suite and published beside the pinned pair, so the gap closes run over run instead
# of waiting for the next recording (docs/design/convergence-fixture-fill.md).
#
#   convergence-fill.sh fills  <code> <corpus run>               # the pair's fill assets, oldest first, one line
#   convergence-fill.sh unpack <stage dir> [fill asset...]       # each laid over a staged tree
#   convergence-fill.sh gaps   <code> <corpus run> <out file>    # the newest refetch list, less what was refused lately; prints its line count
#   convergence-fill.sh pack   <fill dir> <archive>              # what a fill fetched; no archive when nothing
#   convergence-fill.sh fold   <code> <corpus run>               # fold the pair's fills into one, past CONVERGENCE_FILL_FOLD_AT
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
# FOLDED, SO A PINNED PAIR'S FILLS STAY BOUNDED. A pair kept for good ($KINOWO_KEPT_RECORDINGS) is never pruned, and every
# leg adds a fill to it: run 41351315090 found the release at GitHub's 1000-asset cap (278 of them fills of pair 573) and
# every publish after it refused (HTTP 422 "file_count limited to 1000 assets per release"). Past
# CONVERGENCE_FILL_FOLD_AT fills since the newest fold, `fold` lays them over one another as `unpack` does and publishes
# the result as `fill-<code>-<corpus>-<run id>-<attempt>-folded.tar.zst`, under the newest one's run id and attempt, so
# it sorts after every fill it holds. `fills` reads from the newest fold on: the fold, then each fill after it -- the same
# tree as laying every fill. A fold deletes only what the fold BEFORE it holds (that fold kept): a leg that listed the
# fills before this fold existed still finds every asset it named.
#
# A release it cannot LIST, asked three times, fails `fills` and `gaps` loudly: a failed listing is not an empty
# release, and read as "no fills" it would let the leg decide its verdict on — and pin into its bisect's pair — a tree
# without the fills its pair has. Only a listing that succeeds and names none means no fills. A fill it cannot DOWNLOAD
# or unpack, asked three times, fails `unpack` the same way, for the same reason: the leg's pair names it, so a leg
# replaying without it would decide its verdict on — and hand its bisect — a pair it never replayed.
set -uo pipefail

TAG="${FIXTURE_RELEASE_TAG:?FIXTURE_RELEASE_TAG names the rolling release}"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/tool-conventions.sh
. "$here/../../scripts/tool-conventions.sh" # bounded, gh_download
# Every gh call is bounded: a hung read or download is a failed attempt (retried as any other), never a leg that waits
# out its job's timeout.
GH_READ="${CONVERGENCE_FILL_GH_TIMEOUT:-120}"
GH_DOWNLOAD="${CONVERGENCE_FILL_DOWNLOAD_TIMEOUT:-1200}"
# How many of the newest refused lists a fill skips the gaps of: a gap an origin refused is asked again only once
# this many later fills have published lists without it — never every run through paid egress.
REFUSED_KEPT="${CONVERGENCE_FILL_REFUSED_KEPT:-4}"

# Asked up to three times, the wait doubling from CONVERGENCE_FILL_LIST_BACKOFF seconds (5): one GitHub 5xx is not a
# release that cannot be read, and failing the leg on it re-runs a whole convergence row.
attempts=3
backoff() { sleep "$(( ${CONVERGENCE_FILL_LIST_BACKOFF:-5} << ($1 - 1) ))"; }
# Every download goes through gh_download (bounded by GH_DOWNLOAD, a failed read asked again), on the same back-off.
export GH_DOWNLOAD_BACKOFF="${CONVERGENCE_FILL_LIST_BACKOFF:-5}"

# Every asset name in the release, one per line; an error and a non-zero status when it cannot be listed.
assets() {
  local names attempt=1
  until names=$(bounded "$GH_READ" gh release view "$TAG" --json assets --jq '.assets[].name'); do
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

# The fills a leg lays, one per line: from the newest fold on (every fill when there is none).
from_fold() { awk '{ line[NR] = $0 } /-folded\.tar\.zst$/ { from = NR } END { for (i = (from ? from : 1); i <= NR; i++) if (line[i] != "") print line[i] }'; }

case "${1:-}" in
  fills)
    code="${2:?code}"; corpus="${3:?corpus run}"
    names=$(named fill "$code" "$corpus" 'tar\.zst') || exit 1
    printf '%s\n' "$names" | from_fold | paste -sd' ' -
    ;;
  fold)
    code="${2:?code}"; corpus="${3:?corpus run}"; at="${CONVERGENCE_FILL_FOLD_AT:-12}"
    names=$(named fill "$code" "$corpus" 'tar\.zst') || exit 1
    laid=$(printf '%s\n' "$names" | from_fold)
    count=$(printf '%s' "$laid" | grep -c . || true)
    if [ "$count" -le "$at" ]; then echo "pair $code-$corpus: $count fill(s) since its last fold, at most $at -- not folded"; exit 0; fi
    newest=$(printf '%s\n' "$laid" | tail -1)
    run_id=$(sed -E "s/^fill-$code-$corpus-([0-9]+).*/\\1/" <<< "$newest")
    attempt=$(sed -nE "s/^fill-$code-$corpus-[0-9]+-([0-9]+)([-.].*)?$/\\1/p" <<< "$newest")
    fold="fill-$code-$corpus-$run_id-${attempt:-0}-folded.tar.zst"
    work=$(mktemp -d)
    # shellcheck disable=SC2086 # one asset name a word
    "$0" unpack "$work/tree" $laid > /dev/null || { rm -rf "$work"; echo "::warning::could not restore pair $code-$corpus's fills to fold them"; exit 1; }
    "$0" pack "$work/tree" "$work/$fold" > /dev/null || { rm -rf "$work"; exit 1; }
    # A name is never replaced: an upload refused because a concurrent fold published this very name is that fold done.
    if ! "$here/../../scripts/ci/gh-release.sh" upload "$TAG" "$work/$fold" > /dev/null 2>&1; then
      assets | grep -qxF "$fold" || { rm -rf "$work"; echo "::warning::could not publish $fold"; exit 1; }
    fi
    rm -rf "$work"
    echo "folded $count fill(s) of pair $code-$corpus into $fold"
    # Everything before the PREVIOUS fold: that fold holds it, and stays for the legs that listed before this one.
    previous=$(printf '%s\n' "$names" | grep -n -- '-folded\.tar\.zst$' | tail -1 | cut -d: -f1)
    [ -n "$previous" ] || exit 0
    # allow-silenced: deleting folded fills is housekeeping; one that fails to delete goes with the next fold.
    printf '%s\n' "$names" | head -n "$((previous - 1))" \
      | while read -r stale; do "$here/../../scripts/ci/gh-release.sh" delete-asset "$TAG" "$stale" -y > /dev/null 2>&1 || true; done
    echo "deleted $((previous - 1)) fill(s) the fold before it holds"
    ;;
  unpack)
    stage="${2:?stage dir}"; shift 2
    [ "$#" -gt 0 ] || exit 0
    downloads="$stage.fills"
    mkdir -p "$downloads" "$stage"
    status=0
    for fill in "$@"; do
      attempt=1
      # One attempt a turn: this loop asks again itself, the download and its unpack together. Its messages are SAID:
      # run 41228572378's de-2 spent its whole 900 s tree-restore bound on one fill's rate-limited retries, silenced
      # here, and its log ended at the fill before with nothing to say why.
      until GH_DOWNLOAD_ATTEMPTS=1 gh_download "$GH_DOWNLOAD" release download "$TAG" --pattern "$fill" --dir "$downloads" --clobber &&
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
    if [ -n "$newest" ] && gh_download "$GH_DOWNLOAD" release download "$TAG" --pattern "$newest" --output "$out" --clobber 2>/dev/null; then
      echo "::notice::$newest lists $(grep -c "$(printf '\t')" "$out" || true) fetchable gap(s)" >&2
      skip="$out.refused"
      : > "$skip"
      for list in $refused; do
        # allow-silenced: a refused list that cannot be read only means its gaps are asked again.
        gh_download "$GH_DOWNLOAD" release download "$TAG" --pattern "$list" --output "$skip.part" --clobber 2>/dev/null && cat "$skip.part" >> "$skip"
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
    echo "usage: $0 fills <code> <corpus> | unpack <stage> [asset...] | gaps <code> <corpus> <out> | pack <dir> <archive> | fold <code> <corpus>" >&2
    exit 64 ;;
esac

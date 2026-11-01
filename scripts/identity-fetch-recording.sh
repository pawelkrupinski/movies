#!/usr/bin/env bash
#
# Lay out ONE "Record scrape fixtures" run's recording beside the checkout, not in it — each
# country's scrape corpus, the enrichment tree recorded with it, and its identity overlay when the
# release holds one — under <dest>/pair as the repo's own tree (test/resources/fixtures/{corpus,
# enrichment-<cc>}), to replay or diff an older recording against today's. scripts/identity-capture.sh
# and scripts/convergence-local.sh fetch only the newest (or pinned) recording, into the checkout.
#
#   scripts/identity-fetch-recording.sh [--corpus-only | --no-overlay] [--fills "<asset> ..."] <recording run id> <dest dir> [cc ...]
#                                                                         (default: pl de uk us es)
#   --corpus-only  the scrape corpus alone: no enrichment tree, no overlay
#   --no-overlay   the corpus and enrichment tree, without the identity overlay (what the decorations relearn reads)
#   --fills        the fill assets a hermetic leg laid over its tree, as its log names them ("fills: fill-uk-573-655-1-
#                  convergence.tar.zst ..."): each of a laid-out country's (fill-<cc>-...) is laid over its tree in order,
#                  a later file winning, as .github/scripts/convergence-fill.sh unpack lays them -- so a replay reads the
#                  answers that leg read. A named fill the release does not hold fails the fetch.
#
# Each country's corpus is laid out LAST, its cinema-scrapes-<cc>.json.gz moved in by a rename: that file
# there means the country is laid out whole (identity_recorded in scripts/identity-tools.sh, which
# calls this one download at a time). Every gh download goes through tool-conventions.sh's gh_download: bounded
# (KINOWO_IDENTITY_DOWNLOAD_TIMEOUT seconds, default 1800) and a failed read asked again (GH_DOWNLOAD_ATTEMPTS).
#
# Needs `gh` signed in. KINOWO_REPO (default pawelkrupinski/movies) and FIXTURE_RELEASE_TAG (default
# convergence-fixtures) name where the recording lives. A recording the convergence release no longer holds (it keeps
# the newest five pairs), or one recorded on a forge that no longer holds it (no run, so no artifact, by that id: 38713274617),
# is read from the ARCHIVE release instead (RECORDING_ARCHIVE_TAG, default identity-recordings): its tree under the
# same name, and its corpus as scrapes-<cc>-<run>.tar.*, which is asked first (absent, the run's artifact is read). Archives are zstd or gzip; each is unpacked by
# its magic bytes (.github/scripts/unpack-fixture-archive.sh). Downloads are kept in <dest>/archive.
set -euo pipefail

# shellcheck source=scripts/tool-conventions.sh
. "$(dirname "$0")/tool-conventions.sh"
usage() { usage_from_header; }
case "${1:-}" in -h|--help) usage; exit 0 ;; esac
corpus_only=0 overlay=1 fills=""
while [ "$#" -gt 0 ]; do
  case "$1" in
    --corpus-only) corpus_only=1; shift ;;
    --no-overlay) overlay=0; shift ;;
    --fills) [ "$#" -ge 2 ] || { usage >&2; exit 2; }; fills=${2//,/ }; shift 2 ;;
    *) break ;;
  esac
done
[ "$#" -ge 2 ] || { usage >&2; exit 2; }

RUN=$1 DEST=$2
shift 2
CCS=("$@")
[ "${#CCS[@]}" -gt 0 ] || CCS=("${KINOWO_COUNTRIES[@]}")
REPO=$KINOWO_GH_REPO
RELEASE=${FIXTURE_RELEASE_TAG:-convergence-fixtures}
ARCHIVE=${RECORDING_ARCHIVE_TAG:-identity-recordings}
UNPACK="$(cd "$(dirname "$0")/.." && pwd)/.github/scripts/unpack-fixture-archive.sh"
LIMIT=${KINOWO_IDENTITY_DOWNLOAD_TIMEOUT:-1800}
mkdir -p "$DEST/pair" "$DEST/archive"

# fetch <asset prefix> [release] -> the downloaded asset's path, whichever compressor packed it; 1 when the release
# lacks it, 2 when the release could not be read (gh_download tells the two apart, asking a failed read again; a
# timeout is one)
fetch() {
  local ext status
  for ext in tar.zst tar.gz; do
    # absent (this compressor did not pack it) moves on to the next; a failed read stops the fetch
    gh_download "$LIMIT" release download "${2:-$RELEASE}" -R "$REPO" --pattern "$1.$ext" --dir "$DEST/archive" --clobber >&2
    status=$?
    case $status in
      0) echo "$DEST/archive/$1.$ext"; return 0 ;;
      3) ;;
      *) return 2 ;;
    esac
  done
  return 1
}

for cc in "${CCS[@]}"; do
  if [ "$corpus_only" = 0 ]; then
    if tree=$(fetch "enrichment-$cc-$RUN"); then :
    elif [ $? -eq 2 ]; then echo "$cc: could not read release $RELEASE (see gh's error above)" >&2; exit 1
    elif tree=$(fetch "enrichment-$cc-$RUN" "$ARCHIVE"); then echo "$cc: tree from the archive release $ARCHIVE" >&2
    elif [ $? -eq 2 ]; then echo "$cc: could not read release $ARCHIVE (see gh's error above)" >&2; exit 1
    else echo "$cc: neither release $RELEASE nor $ARCHIVE holds enrichment-$cc-$RUN" >&2; exit 1; fi
    bash "$UNPACK" "$tree" "$DEST/pair"
    # the overlay is optional: absent is fine, a failed read is not
    if [ "$overlay" = 0 ]; then :
    elif archive=$(fetch "identity-overlay-$cc-$RUN"); then bash "$UNPACK" "$archive" "$DEST/pair"
    elif [ $? -eq 2 ]; then echo "$cc: could not read release $RELEASE (see gh's error above)" >&2; exit 1; fi
    for fill in $fills; do
      case "$fill" in fill-"$cc"-*) ;; *) continue ;; esac
      if gh_download "$LIMIT" release download "$RELEASE" -R "$REPO" --pattern "$fill" --dir "$DEST/archive" --clobber >&2
      then status=0; else status=$?; fi
      case $status in
        0) bash "$UNPACK" "$DEST/archive/$fill" "$DEST/pair"; echo "$cc: laid fill $fill over the tree" >&2 ;;
        3) echo "$cc: release $RELEASE holds no fill $fill" >&2; exit 1 ;;
        *) echo "$cc: could not read release $RELEASE for fill $fill (see gh's error above)" >&2; exit 1 ;;
      esac
    done
  fi
  # a download cut short earlier leaves files gh will not overwrite
  rm -rf "$DEST/archive/scrape-$cc"; mkdir -p "$DEST/archive/scrape-$cc"
  if corpus=$(fetch "scrapes-$cc-$RUN" "$ARCHIVE"); then echo "$cc: corpus from the archive release $ARCHIVE" >&2
  elif [ $? -eq 2 ]; then echo "$cc: could not read release $ARCHIVE (see gh's error above)" >&2; exit 1
  else
    gh_download "$LIMIT" run download "$RUN" -R "$REPO" --name "scrape-fixtures-$cc" --dir "$DEST/archive/scrape-$cc" ||
      { echo "$cc: could not download run $RUN's scrape-fixtures-$cc (gh failed or timed out)" >&2; exit 1; }
    corpus=$(compgen -G "$DEST/archive/scrape-$cc/scrapes-$cc.tar.*" | head -1 || true)
  fi
  [ -n "$corpus" ] || { echo "$cc: run $RUN's scrape-fixtures-$cc holds no scrapes-$cc.tar.*" >&2; exit 1; }
  # unpacked beside the pair, then moved in: cinema-scrapes-<cc>.json.gz is renamed in last
  rm -rf "$DEST/archive/corpus-$cc"
  bash "$UNPACK" "$corpus" "$DEST/archive/corpus-$cc"
  unpacked=$DEST/archive/corpus-$cc/test/resources/fixtures/corpus
  [ -f "$unpacked/cinema-scrapes-$cc.json.gz" ] || { echo "$cc: run $RUN's scrapes-$cc archive holds no cinema-scrapes-$cc.json.gz" >&2; exit 1; }
  mkdir -p "$DEST/pair/test/resources/fixtures/corpus"
  for f in "$unpacked"/*; do
    [ "$(basename "$f")" = "cinema-scrapes-$cc.json.gz" ] || mv -f "$f" "$DEST/pair/test/resources/fixtures/corpus/"
  done
  mv -f "$unpacked/cinema-scrapes-$cc.json.gz" "$DEST/pair/test/resources/fixtures/corpus/"
  rm -rf "$DEST/archive/corpus-$cc"
  echo "$cc: $(du -sh "$DEST/pair" | cut -f1) laid out so far"
done

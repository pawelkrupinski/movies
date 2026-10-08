#!/usr/bin/env bash
#
# Lay out ONE "Record scrape fixtures" run's recording beside the checkout, not in it — each
# country's scrape corpus, the enrichment tree recorded with it, and its identity overlay when the
# release holds one — under <dest>/pair as the repo's own tree (test/resources/fixtures/{corpus,
# enrichment-<cc>}), to replay or diff an older recording against today's. scripts/identity-capture.sh
# and scripts/convergence-local.sh fetch only the newest (or pinned) recording, into the checkout.
#
#   scripts/identity-fetch-recording.sh <recording run id> <dest dir> [cc ...]   (default: pl de uk us es)
#
# Needs `gh` signed in. KINOWO_REPO (default pawelkrupinski/movies) and FIXTURE_RELEASE_TAG (default
# convergence-fixtures) name where the recording lives. Archives are zstd or gzip; each is unpacked by
# its magic bytes (.github/scripts/unpack-fixture-archive.sh). Downloads are kept in <dest>/archive.
set -euo pipefail

usage() { sed -n '3,/^set /p' "$0" | sed '$d; s/^# \{0,1\}//'; }
case "${1:-}" in -h|--help) usage; exit 0 ;; esac
[ "$#" -ge 2 ] || { usage >&2; exit 2; }

RUN=$1 DEST=$2
shift 2
CCS=("$@")
[ "${#CCS[@]}" -gt 0 ] || CCS=(pl de uk us es)
REPO=${KINOWO_REPO:-pawelkrupinski/movies}
RELEASE=${FIXTURE_RELEASE_TAG:-convergence-fixtures}
UNPACK="$(cd "$(dirname "$0")/.." && pwd)/.github/scripts/unpack-fixture-archive.sh"
GH_OPTIONAL="$(cd "$(dirname "$0")" && pwd)/ci/gh-optional.sh"
mkdir -p "$DEST/pair" "$DEST/archive"

# fetch <asset prefix> -> the downloaded asset's path, whichever compressor packed it; 1 when the release lacks it,
# 2 when the release could not be read (gh-optional.sh tells the two apart)
fetch() {
  local ext present
  for ext in tar.zst tar.gz; do
    # absent (this compressor did not pack it) moves on to the next; a failed read stops the fetch
    present=$("$GH_OPTIONAL" release download "$RELEASE" -R "$REPO" --pattern "$1.$ext" --dir "$DEST/archive" --clobber) || return 2
    if [ "$present" = present ]; then
      echo "$DEST/archive/$1.$ext"
      return 0
    fi
  done
  return 1
}

for cc in "${CCS[@]}"; do
  if tree=$(fetch "enrichment-$cc-$RUN"); then :
  elif [ $? -eq 2 ]; then echo "$cc: could not read release $RELEASE (see gh's error above)" >&2; exit 1
  else echo "$cc: release $RELEASE holds no enrichment-$cc-$RUN" >&2; exit 1; fi
  bash "$UNPACK" "$tree" "$DEST/pair"
  # the overlay is optional: absent is fine, a failed read is not
  if overlay=$(fetch "identity-overlay-$cc-$RUN"); then bash "$UNPACK" "$overlay" "$DEST/pair"
  elif [ $? -eq 2 ]; then echo "$cc: could not read release $RELEASE (see gh's error above)" >&2; exit 1; fi
  gh run download "$RUN" -R "$REPO" --name "scrape-fixtures-$cc" --dir "$DEST/archive/scrape-$cc"
  corpus=$(compgen -G "$DEST/archive/scrape-$cc/scrapes-$cc.tar.*" | head -1 || true)
  [ -n "$corpus" ] || { echo "$cc: run $RUN's scrape-fixtures-$cc holds no scrapes-$cc.tar.*" >&2; exit 1; }
  bash "$UNPACK" "$corpus" "$DEST/pair"
  echo "$cc: $(du -sh "$DEST/pair" | cut -f1) laid out so far"
done

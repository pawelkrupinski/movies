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
mkdir -p "$DEST/pair" "$DEST/archive"

# fetch <asset prefix> -> the downloaded asset's path, whichever compressor packed it; fails when the release lacks it
fetch() {
  local ext
  for ext in tar.zst tar.gz; do
    if gh release download "$RELEASE" -R "$REPO" --pattern "$1.$ext" --dir "$DEST/archive" --clobber 2>/dev/null; then
      echo "$DEST/archive/$1.$ext"
      return 0
    fi
  done
  return 1
}

for cc in "${CCS[@]}"; do
  tree=$(fetch "enrichment-$cc-$RUN") || { echo "$cc: release $RELEASE holds no enrichment-$cc-$RUN" >&2; exit 1; }
  bash "$UNPACK" "$tree" "$DEST/pair"
  if overlay=$(fetch "identity-overlay-$cc-$RUN"); then bash "$UNPACK" "$overlay" "$DEST/pair"; fi
  gh run download "$RUN" -R "$REPO" --name "scrape-fixtures-$cc" --dir "$DEST/archive/scrape-$cc"
  corpus=$(compgen -G "$DEST/archive/scrape-$cc/scrapes-$cc.tar.*" | head -1 || true)
  [ -n "$corpus" ] || { echo "$cc: run $RUN's scrape-fixtures-$cc holds no scrapes-$cc.tar.*" >&2; exit 1; }
  bash "$UNPACK" "$corpus" "$DEST/pair"
  echo "$cc: $(du -sh "$DEST/pair" | cut -f1) laid out so far"
done

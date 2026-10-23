#!/usr/bin/env bash
#
# Copy "Record scrape fixtures" recordings into the permanent archive release (.github/workflows/archive-recording.yml).
#
#   scripts/ci/archive-recording.sh <run> [<run> ...]
#
# Per run and country (KINOWO_COUNTRIES): enrichment-<cc>-<run>.tar.* from $FIXTURE_RELEASE_TAG (default
# convergence-fixtures) and the run's scrape-fixtures-<cc> artifact's scrapes-<cc>.tar.* as scrapes-<cc>-<run>.tar.*,
# both uploaded to $RECORDING_ARCHIVE_TAG (default identity-recordings). An asset the archive holds at the source's
# size is skipped (a partial upload, at another size, is replaced); a country the recording lacks is said and skipped.
# Exit 1 when any copy failed or a run left nothing archived. Needs `gh` (the shim on Forgejo) with release write.
set -uo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/tool-conventions.sh
. "$here/../tool-conventions.sh" # KINOWO_COUNTRIES
[ "$#" -gt 0 ] || { echo "usage: $0 <run> [<run> ...]" >&2; exit 64; }
SOURCE=${FIXTURE_RELEASE_TAG:-convergence-fixtures}
ARCHIVE=${RECORDING_ARCHIVE_TAG:-identity-recordings}
RELEASE="$here/gh-release.sh"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# "<name>\t<size>" for every asset of a release
sizes() { gh release view "$1" --json assets --jq '.assets[] | "\(.name)\t\(.size)"'; }
size_of() { awk -F'\t' -v n="$2" '$1 == n { print $2 }' <<< "$1"; }

source_assets=$(sizes "$SOURCE") || { echo "::error::could not read release $SOURCE"; exit 1; }
archived=$(sizes "$ARCHIVE") || { echo "::error::could not read release $ARCHIVE"; exit 1; }

# upload <file> <name>: into the archive unless it holds <name> at this size already
upload() {
  local have want
  want=$(wc -c < "$1" | tr -d ' ') have=$(size_of "$archived" "$2")
  if [ "$have" = "$want" ]; then echo "  $2: archived already ($want bytes)"; return 0; fi
  [ "$1" -ef "$work/$2" ] || ln -f "$1" "$work/$2" 2>/dev/null || cp "$1" "$work/$2"
  "$RELEASE" upload "$ARCHIVE" "$work/$2" --clobber || { echo "::error::could not upload $2 to $ARCHIVE"; return 1; }
  echo "  $2: archived ($want bytes)"
}

failed=0
for run in "$@"; do
  [[ "$run" =~ ^[0-9]+$ ]] || { echo "::error::not a run id: $run"; failed=1; continue; }
  echo "recording $run"
  copied=0
  for cc in "${KINOWO_COUNTRIES[@]}"; do
    tree=$(grep -oE "^enrichment-$cc-$run\\.tar\\.(zst|gz)" <<< "$source_assets" | head -1)
    if [ -z "$tree" ] && grep -qE "^enrichment-$cc-$run\\.tar\\.(zst|gz)	" <<< "$archived"; then
      echo "  enrichment-$cc-$run: only in $ARCHIVE already"; copied=$((copied + 1))
    elif [ -z "$tree" ]; then
      echo "  $cc: $SOURCE holds no enrichment-$cc-$run -- not a recording of $cc (or already pruned): skipped"
      continue
    elif [ "$(size_of "$archived" "$tree")" = "$(size_of "$source_assets" "$tree")" ]; then
      echo "  $tree: archived already"; copied=$((copied + 1))
    else
      rm -rf "$work/dl"; mkdir -p "$work/dl"
      gh release download "$SOURCE" --pattern "$tree" --dir "$work/dl" --clobber &&
        upload "$work/dl/$tree" "$tree" && copied=$((copied + 1)) || failed=1
      rm -rf "$work/dl" "${work:?}/$tree"
    fi
    if grep -qE "^scrapes-$cc-$run\\.tar\\.(zst|gz)	" <<< "$archived"; then
      echo "  scrapes-$cc-$run: archived already"; continue
    fi
    rm -rf "$work/art"; mkdir -p "$work/art"
    if ! gh run download "$run" --name "scrape-fixtures-$cc" --dir "$work/art"; then
      echo "::error::recording $run holds a $cc tree but its scrape-fixtures-$cc artifact could not be read"
      failed=1; continue
    fi
    corpus=$(find "$work/art" -name "scrapes-$cc.tar.*" | head -1)
    [ -n "$corpus" ] || { echo "::error::$run's scrape-fixtures-$cc holds no scrapes-$cc.tar.*"; failed=1; continue; }
    upload "$corpus" "scrapes-$cc-$run.${corpus##*/scrapes-"$cc".}" || failed=1
    rm -rf "$work/art" "$work/scrapes-$cc-$run".*
  done
  [ "$copied" -gt 0 ] || { echo "::error::recording $run: nothing archived (no tree in $SOURCE or $ARCHIVE)"; failed=1; }
done
exit "$failed"

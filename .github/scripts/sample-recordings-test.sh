#!/usr/bin/env bash
# sample-recordings.sh: a sample row's recordings packed on one runner, merged into another's tree.
# Run: bash .github/scripts/sample-recordings-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m sample-recordings.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
script="$REPO_ROOT/.github/scripts/sample-recordings.sh"
tree="test/resources/fixtures/enrichment-us"

# The SAMPLE row: a restored tree (old mtimes), a stamp, then what the sample writes.
mkdir -p "$work/sample/$tree/tmdb" && cd "$work/sample" || exit 1
printf 'restored\n' > "$tree/tmdb/untouched";  touch -t 202601010000 "$tree/tmdb/untouched"
touch "$work/stamp"; sleep 1
printf 'sample-only\n' > "$tree/tmdb/only-sample"
printf 'sample-fresh\n' > "$tree/tmdb/stale-here"
printf 'sample-older\n' > "$tree/tmdb/both"
printf 'sample-leg\n' > "$tree/.identity-lookups-v3"
bash "$script" pack "$tree" "$work/stamp" "$work/recordings.tar.zst" > /dev/null
check "pack takes only what the sample wrote" "4" "$(zstd -dc "$work/recordings.tar.zst" | tar -tf - | grep -vc '/$')"

# The CONVERGENCE row: its own restored tree, its own later recordings.
mkdir -p "$work/conv/$tree/tmdb" && cd "$work/conv" || exit 1
printf 'restored-old\n' > "$tree/tmdb/stale-here"; touch -t 202601010000 "$tree/tmdb/stale-here"
sleep 1; printf 'suite-newer\n' > "$tree/tmdb/both"
printf 'full-leg\n' > "$tree/.identity-lookups-v3"
bash "$script" merge "$work/recordings.tar.zst" > /dev/null
check "a file only the sample recorded is taken" "sample-only" "$(cat "$tree/tmdb/only-sample")"
check "the sample's fresh answer replaces an older one here" "sample-fresh" "$(cat "$tree/tmdb/stale-here")"
check "an answer recorded here after the sample's is kept" "suite-newer" "$(cat "$tree/tmdb/both")"
check "the identity marker names both legs" "full-leg sample-leg" "$(tr '\n' ' ' < "$tree/.identity-lookups-v3" | sed 's/ $//')"

# An OVERLAY merge: the convergence row's overlay stamp is LATER than the sample's fill (their
# setups ran side by side), and the overlay publish packs only files newer than that stamp.
mkdir -p "$work/overlay/$tree" && cd "$work/overlay" || exit 1
touch "$work/overlay-stamp"
bash "$script" merge "$work/recordings.tar.zst" > /dev/null
check "without a stamp, a taken file keeps the sample's mtime" "" "$(find "$tree" -type f -newer "$work/overlay-stamp")"
rm -rf "${work:?}/overlay/$tree" && mkdir -p "$work/overlay/$tree"
bash "$script" merge "$work/recordings.tar.zst" "$work/overlay-stamp" > /dev/null
check "with an overlay stamp, every taken file is newer than it" "4" "$(find "$tree" -type f -newer "$work/overlay-stamp" | wc -l | tr -d ' ')"

spec_summary

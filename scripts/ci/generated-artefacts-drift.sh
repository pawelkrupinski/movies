#!/usr/bin/env bash
#
# Regenerate every checked-in GENERATED artefact from its source, and fail if that changes
# anything: the committed output has drifted from what its generator makes today.
#
# The roster reaches the apps through a chain of generators, each checked in as its output:
#
#   Cinema.scala's venue annotations ("// Biecz — filmweb 2315")
#     → data/pl/scripts/build_venue_towns.py  → data/pl/venues.json
#     → data/pl/scripts/build_pages.py        → data/pl/pages.json
#     → data/pl/scripts/generate_polish_pages.py → PolishPages.scala + the apps' City lists
#   data/{uk,pl}/venues.json → data/scripts/generate_venue_towns.py → VenueTowns.scala
#   data/us/venues.json      → data/us/scripts/generate_roster.py   → UsRosterData.scala
#   data/germany, data/spain → their generate_roster.py              → *RosterData.scala
#
# An edit to any link that skips the regeneration below it compiles, passes every spec that
# reads the stale output, and ships a roster that disagrees with itself. (The catalog seeds
# the apps boot from are the next link; CatalogSeedSpec guards those in the unit run.)
#
# Only the offline steps run here. The UK's venues.json is harvested live from Flicks
# (harvest_towns.py) and is an input, not an output. GeoNames' Polish gazetteer is fetched
# because build_venue_towns.py validates town names and build_pages.py reads coordinates
# from it; it is deleted again afterwards and never committed.
#
# Locally:  scripts/ci/generated-artefacts-drift.sh   (then commit what it rewrote)
#
# A DIRTY WORKING TREE IS FINE: drift is what the generators CHANGE, not what differs from HEAD.
# The tree's state is fingerprinted before they run and compared after, so the pre-push hook
# (scripts/hooks/pre-push) can run this over a tree with unrelated edits in it. In CI the tree
# starts clean and the two readings are the same thing.
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

# The generators import each other (data/us/scripts), and CPython would drop __pycache__/
# next to them — untracked files the drift check below would then report as drift.
export PYTHONDONTWRITEBYTECODE=1

# The US centroid has to be the same number on every Python (see its test); prove that
# before trusting a comparison with the committed roster.
python3 data/us/scripts/test_generate_roster.py

geonames=data/pl/geonames

# "<path> <blob id>" for every file that differs from HEAD or is untracked (GeoNames excluded).
tree_state() {
    git status --porcelain=v1 -z --untracked-files=all -- . ":(exclude)$geonames" | tr '\0' '\n' \
        | sed -n 's/^.. //p' | LC_ALL=C sort -u | while IFS= read -r path; do
            if [ -f "$path" ]; then echo "$path $(git hash-object -- "$path")"; else echo "$path deleted"; fi
        done
}
before=$(tree_state)
fetched_geonames=false
# GEONAMES_CACHE (optional): a directory to keep PL.txt in between runs. The pre-push hook sets
# it, so only its first drift check pays the download; CI leaves it unset and fetches fresh.
if [ ! -f "$geonames/PL.txt" ]; then
    mkdir -p "$geonames"
    if [ -n "${GEONAMES_CACHE:-}" ] && [ -f "$GEONAMES_CACHE/PL.txt" ]; then
        cp "$GEONAMES_CACHE/PL.txt" "$geonames/PL.txt"
    else
        curl -fsSL --retry 5 --retry-all-errors --connect-timeout 20 --max-time 300 \
            https://download.geonames.org/export/dump/PL.zip -o "$geonames/PL.zip"
        unzip -oq "$geonames/PL.zip" -d "$geonames"
        if [ -n "${GEONAMES_CACHE:-}" ]; then
            mkdir -p "$GEONAMES_CACHE" && cp "$geonames/PL.txt" "$GEONAMES_CACHE/PL.txt"
        fi
    fi
    fetched_geonames=true
fi
cleanup() { if [ "$fetched_geonames" = true ]; then rm -rf "$geonames"; fi; }
trap cleanup EXIT

python3 data/pl/scripts/build_venue_towns.py
python3 data/pl/scripts/build_pages.py
python3 data/pl/scripts/generate_polish_pages.py
python3 data/scripts/generate_venue_towns.py
python3 data/us/scripts/generate_roster.py data/us/venues.json common/src/main/scala/models/UsRosterData.scala
python3 data/germany/scripts/generate_roster.py
python3 data/spain/scripts/generate_roster.py

drift=$(diff <(printf '%s\n' "$before") <(tree_state) | sed -n 's/^[<>] \(.*\) [^ ]*$/\1/p' | LC_ALL=C sort -u || true)
if [ -n "$drift" ]; then
    echo "::error::Generated artefacts are stale — their generators rewrote them. Run scripts/ci/generated-artefacts-drift.sh and commit the result:"
    echo "$drift"
    # shellcheck disable=SC2086 # one path per line, none with spaces
    git --no-pager diff --stat -- $drift
    # shellcheck disable=SC2086
    git --no-pager diff -- $drift | head -200
    exit 1
fi
echo "Every generated artefact matches what its generator makes."

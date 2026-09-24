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
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

# The generators import each other (data/us/scripts), and CPython would drop __pycache__/
# next to them — untracked files the drift check below would then report as drift.
export PYTHONDONTWRITEBYTECODE=1

# The US centroid has to be the same number on every Python (see its test); prove that
# before trusting a comparison with the committed roster.
python3 data/us/scripts/test_generate_roster.py

geonames=data/pl/geonames
fetched_geonames=false
if [ ! -f "$geonames/PL.txt" ]; then
    mkdir -p "$geonames"
    curl -fsSL --retry 5 --retry-all-errors --connect-timeout 20 --max-time 300 \
        https://download.geonames.org/export/dump/PL.zip -o "$geonames/PL.zip"
    unzip -oq "$geonames/PL.zip" -d "$geonames"
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

drift=$(git status --porcelain --untracked-files=all -- . ":(exclude)$geonames")
if [ -n "$drift" ]; then
    echo "::error::Generated artefacts are stale — their generators rewrote them. Run scripts/ci/generated-artefacts-drift.sh and commit the result:"
    echo "$drift"
    git --no-pager diff --stat
    git --no-pager diff | head -200
    exit 1
fi
echo "Every generated artefact matches what its generator makes."

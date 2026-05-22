#!/usr/bin/env bash
# Compile and run the FilmDetailParser smoke test.
# Usage:
#   ./Tools/run_film_smoke.sh                       # fetches /film for the listing's first title
#   ./Tools/run_film_smoke.sh "Title with spaces"   # fetches /film?title=<title>
#   ./Tools/run_film_smoke.sh path.html             # uses local HTML
set -euo pipefail
cd "$(dirname "$0")/.."

OUT="$(mktemp -t kinowo_film_smoke)"
trap 'rm -f "$OUT"' EXIT

swiftc -O -o "$OUT" \
    Tools/FilmDetailParserSmoke.swift \
    Kinowo/Networking/FilmDetailParser.swift \
    Kinowo/Networking/HTMLPrimitives.swift \
    Kinowo/Networking/ShowingsParser.swift \
    Kinowo/Networking/RatingsParser.swift \
    Kinowo/Networking/HTMLDecoding.swift \
    Kinowo/Models/FilmDetail.swift \
    Kinowo/Models/Film.swift

"$OUT" "$@"

#!/usr/bin/env bash
# Compile and run the HTMLParser smoke test.
# Usage:
#   ./Tools/run_smoke.sh              # fetches https://kinowo.fly.dev/
#   ./Tools/run_smoke.sh path.html    # uses local HTML
set -euo pipefail
cd "$(dirname "$0")/.."

OUT="$(mktemp -t kinowo_smoke)"
trap 'rm -f "$OUT"' EXIT

swiftc -O -o "$OUT" \
    Tools/HTMLParserSmoke.swift \
    Kinowo/Networking/HTMLParser.swift \
    Kinowo/Networking/HTMLPrimitives.swift \
    Kinowo/Networking/ShowingsParser.swift \
    Kinowo/Networking/RatingsParser.swift \
    Kinowo/Networking/HTMLDecoding.swift \
    Kinowo/Models/Film.swift

"$OUT" "$@"

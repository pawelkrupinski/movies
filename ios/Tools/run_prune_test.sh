#!/usr/bin/env bash
# Compile and run the prune-past-showings unit test.
set -euo pipefail
cd "$(dirname "$0")/.."

OUT="$(mktemp -t kinowo_prune_test)"
trap 'rm -f "$OUT"' EXIT

# Filters.swift carries `prunedPastShowings` + `ShowtimeClock`; Film.swift
# holds the data shapes. No SwiftUI / UIKit needed — pure Foundation.
swiftc -O -o "$OUT" \
    Tools/PrunePastShowingsTest.swift \
    Kinowo/Models/Film.swift \
    Kinowo/Models/Filters.swift

"$OUT"

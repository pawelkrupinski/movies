#!/usr/bin/env bash
# Compile and run the FlowLayoutMath unit-test driver.
set -euo pipefail
cd "$(dirname "$0")/.."

OUT="$(mktemp -t kinowo_flowlayout_test)"
trap 'rm -f "$OUT"' EXIT

# FlowLayout.swift defines both the SwiftUI Layout and the pure
# FlowLayoutMath helper. The Layout side needs SwiftUI; the math side
# only needs CoreGraphics. We compile against SwiftUI so the file
# compiles as-is and the test driver picks up only the math part.
swiftc -O -o "$OUT" \
    -framework SwiftUI \
    Tools/FlowLayoutTest.swift \
    Kinowo/Views/FlowLayout.swift

"$OUT"

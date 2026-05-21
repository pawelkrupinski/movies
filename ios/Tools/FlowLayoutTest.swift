// Unit-test driver for `FlowLayoutMath`. Compiles the layout helper
// against synthetic CGSize inputs and asserts the wrap / placement
// behaviour matches expectations — guards against the rounding bug
// that put RT 61% on line 2 of the Mandalorian + Grogu card (fractional
// widths summing to maxWidth + 0.x and the wrap check tripping).
//
// Usage:
//   ./Tools/run_flowlayout_test.sh

import CoreGraphics
import Foundation

@main
enum FlowLayoutTest {
    static func main() {
        var failures = 0
        func check(_ name: String, _ cond: Bool, _ extra: String = "") {
            if cond { print("✓ \(name)") }
            else { print("✗ \(name) \(extra)"); failures += 1 }
        }

        // ── Case 1: three pills that comfortably fit one line.
        do {
            let sizes = [
                CGSize(width: 50, height: 16),  // IMDb 7.0
                CGSize(width: 22, height: 16),  // MC 54
                CGSize(width: 44, height: 16),  // RT 61%
            ]
            let r = FlowLayoutMath.layout(sizes: sizes, maxWidth: 154, spacing: 4, lineSpacing: 4)
            // Expect all three on one line: y == 0 for every position.
            check("three small pills stay on line 1",
                  r.positions.allSatisfy { $0.y == 0 },
                  "positions=\(r.positions)")
            check("three small pills total height = max pill height",
                  r.totalSize.height == 16,
                  "got \(r.totalSize.height)")
        }

        // ── Case 2: the Mandalorian + Grogu regression — three pills
        // whose widths come back from SwiftUI with fractional points,
        // summing to maxWidth + tiny epsilon if NOT rounded. With the
        // ceiling rule, items still fit because maxWidth is bigger
        // than the rounded sum.
        do {
            let sizes = [
                CGSize(width: 50.3, height: 16.4),
                CGSize(width: 22.1, height: 16.4),
                CGSize(width: 44.2, height: 16.4),
            ]
            // Rounded sum = 51 + 23 + 45 + 2 gaps × 4 = 127 pt. maxWidth
            // 130 gives the wrap math just enough headroom.
            let r = FlowLayoutMath.layout(sizes: sizes, maxWidth: 130, spacing: 4, lineSpacing: 4)
            check("fractional-width pills do not wrap when ceil(sum) <= maxWidth",
                  r.positions.allSatisfy { $0.y == 0 },
                  "positions=\(r.positions), maxWidth=130")
        }

        // ── Case 3: four pills, the last one genuinely overflows.
        do {
            let sizes = [
                CGSize(width: 50, height: 16),
                CGSize(width: 22, height: 16),
                CGSize(width: 44, height: 16),
                CGSize(width: 50, height: 16),  // FW 7.2
            ]
            // 50 + 4 + 22 + 4 + 44 + 4 + 50 = 178; maxWidth = 150 forces
            // a wrap before the 4th pill (or possibly the 3rd).
            let r = FlowLayoutMath.layout(sizes: sizes, maxWidth: 150, spacing: 4, lineSpacing: 4)
            // First three pills sum to 50+4+22+4+44 = 124 ≤ 150 → line 1.
            check("first 3 of 4 pills stay on line 1 at maxWidth=150",
                  r.positions[0].y == 0 && r.positions[1].y == 0 && r.positions[2].y == 0,
                  "positions=\(r.positions)")
            // Fourth pill: 124 + spacing + 50 = 178 > 150 → wraps.
            check("4th pill wraps when it would overflow",
                  r.positions[3].y > 0,
                  "y=\(r.positions[3].y)")
        }

        // ── Case 4: a single oversized pill never wraps to a phantom
        // line zero — it lands at the origin and overflows visibly.
        do {
            let sizes = [CGSize(width: 300, height: 16)]
            let r = FlowLayoutMath.layout(sizes: sizes, maxWidth: 100, spacing: 4, lineSpacing: 4)
            check("a single overlong pill stays on line 1 (overflows rather than disappears)",
                  r.positions[0] == .zero,
                  "got \(r.positions[0])")
        }

        // ── Case 5: empty input.
        do {
            let r = FlowLayoutMath.layout(sizes: [], maxWidth: 100, spacing: 4, lineSpacing: 4)
            check("empty input returns zero total size",
                  r.positions.isEmpty && r.totalSize == .zero)
        }

        // ── Case 6: pill widths get rounded UP, so wrap math doesn't
        // false-positively conclude "fits" when each item is 0.4 pt
        // wider than the integer it reports. With three 50.6-pt items
        // and spacing 4, the rendered total = 51 + 4 + 51 + 4 + 51 =
        // 161 pt. maxWidth 161 fits; maxWidth 160 should wrap the
        // third item.
        do {
            let sizes = Array(repeating: CGSize(width: 50.6, height: 16), count: 3)
            let fits   = FlowLayoutMath.layout(sizes: sizes, maxWidth: 161, spacing: 4, lineSpacing: 4)
            let wraps  = FlowLayoutMath.layout(sizes: sizes, maxWidth: 160, spacing: 4, lineSpacing: 4)
            check("ceil(50.6)=51, 3 items fit in 161 pt",
                  fits.positions.allSatisfy { $0.y == 0 },
                  "positions=\(fits.positions)")
            check("ceil(50.6)=51, 3 items wrap before reaching 161 if maxWidth = 160",
                  wraps.positions[2].y > 0,
                  "positions=\(wraps.positions)")
        }

        if failures > 0 {
            print("FAILED with \(failures) check failures")
            exit(2)
        }
        print("ALL OK")
    }
}

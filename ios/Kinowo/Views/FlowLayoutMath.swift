#if canImport(CoreGraphics)
import CoreGraphics

/// Pure layout math behind `FlowLayout`. Lives outside the SwiftUI
/// `Layout` protocol so it can be tested against synthetic `CGSize`
/// inputs without a live SwiftUI rendering pass (the `Subviews`
/// collection is SwiftUI-internal and can't be constructed in a test).
///
/// Kept in a separate file (and behind `#if canImport(CoreGraphics)`)
/// so the test target compiles on Linux Docker CI hosts where SwiftUI
/// AND CoreGraphics are both absent. macOS / iOS still pull it into
/// `KinowoCore`; Linux jobs simply skip it — the unit tests for the
/// algorithm are guarded the same way.
///
/// Algorithm: greedy left-to-right packing, wrap to a new line when the
/// next item plus the trailing inter-item gap won't fit. Inter-item
/// spacing is `spacing`; line spacing is `lineSpacing`.
enum FlowLayoutMath {
    static func layout(
        sizes: [CGSize],
        maxWidth: CGFloat,
        spacing: CGFloat,
        lineSpacing: CGFloat
    ) -> (positions: [CGPoint], totalSize: CGSize) {
        var positions: [CGPoint] = []
        positions.reserveCapacity(sizes.count)
        var x: CGFloat = 0
        var y: CGFloat = 0
        var lineHeight: CGFloat = 0
        var maxLineWidth: CGFloat = 0
        for s in sizes {
            // Round each subview's reported width up to the next whole
            // point. SwiftUI's measurement pass can come back with
            // fractional values that don't survive the round-trip into
            // pixel-aligned drawing: 50.3-pt item proposed at 50.3 pt
            // is fine on a Retina pixel grid, but four 50.3-pt items +
            // three 4-pt gaps sum to 213.2 pt which the wrap check
            // truncates to "doesn't fit in 213" — RT lands on line 2
            // even though the pixels would have rendered cleanly.
            // Ceiling keeps the comparison and the placement honest.
            let w = s.width.rounded(.up)
            let h = s.height.rounded(.up)
            // Wrap when the current line already has content AND the
            // next item would push past `maxWidth`. First item on a
            // line never wraps even if it's wider than `maxWidth`
            // (better to overflow one badge than to render nothing).
            if x > 0, x + w > maxWidth {
                maxLineWidth = max(maxLineWidth, x - spacing)
                x = 0
                y += lineHeight + lineSpacing
                lineHeight = 0
            }
            positions.append(CGPoint(x: x, y: y))
            x += w + spacing
            lineHeight = max(lineHeight, h)
        }
        maxLineWidth = max(maxLineWidth, x - spacing)
        let totalHeight = y + lineHeight
        let width = maxWidth.isFinite ? min(maxWidth, max(maxLineWidth, 0)) : max(maxLineWidth, 0)
        return (positions, CGSize(width: max(0, width), height: max(0, totalHeight)))
    }
}
#endif

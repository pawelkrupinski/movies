import SwiftUI
import CoreGraphics

/// Wraps children onto multiple lines, like `flex-wrap: wrap`.
/// Used for showtime badges + rating pills where the natural HTML behaviour is
/// "flow until the line ends, then continue on the next line".
struct FlowLayout: Layout {
    var spacing: CGFloat = 4
    var lineSpacing: CGFloat = 4

    struct Cache { var size: CGSize = .zero }

    func makeCache(subviews: Subviews) -> Cache { Cache() }
    func updateCache(_ cache: inout Cache, subviews: Subviews) { cache.size = .zero }

    func sizeThatFits(proposal: ProposedViewSize, subviews: Subviews, cache: inout Cache) -> CGSize {
        let sizes = subviews.map { $0.sizeThatFits(.unspecified) }
        return FlowLayoutMath.layout(
            sizes: sizes,
            maxWidth: proposal.width ?? .infinity,
            spacing: spacing,
            lineSpacing: lineSpacing
        ).totalSize
    }

    func placeSubviews(in bounds: CGRect, proposal: ProposedViewSize, subviews: Subviews, cache: inout Cache) {
        let sizes = subviews.map { $0.sizeThatFits(.unspecified) }
        let positions = FlowLayoutMath.layout(
            sizes: sizes,
            maxWidth: bounds.width,
            spacing: spacing,
            lineSpacing: lineSpacing
        ).positions
        for (i, sub) in subviews.enumerated() {
            sub.place(
                at: CGPoint(x: bounds.minX + positions[i].x, y: bounds.minY + positions[i].y),
                anchor: .topLeading,
                // Place every subview at its intrinsic size — not at a
                // measurement-rounded width that could be a fraction of
                // a point shy of what the rendered Text actually wants.
                // That fractional shortfall is what triggers Text's
                // `.tail` truncation ("RT 7…") on iPhone-mini-class
                // grid cells; using `.unspecified` lets each badge
                // render at whatever width it needs.
                proposal: .unspecified
            )
        }
    }
}

/// Pure layout math behind `FlowLayout`. Lives outside the `Layout`
/// protocol so it can be tested against synthetic `CGSize` inputs
/// without a live SwiftUI rendering pass (the `Subviews` collection
/// is SwiftUI-internal and can't be constructed in a test).
///
/// Algorithm: greedy left-to-right packing, wrap to a new line when
/// the next item plus the trailing inter-item gap won't fit. Inter-
/// item spacing is `spacing`; line spacing is `lineSpacing`.
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

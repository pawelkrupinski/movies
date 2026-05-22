import SwiftUI
import CoreGraphics

/// Wraps children onto multiple lines, like `flex-wrap: wrap`.
/// Used for showtime badges + rating pills where the natural HTML behaviour is
/// "flow until the line ends, then continue on the next line".
///
/// All the wrap math lives in `FlowLayoutMath` so it can be unit-tested
/// without a live SwiftUI rendering pass; this file is just the SwiftUI
/// `Layout` shim.
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

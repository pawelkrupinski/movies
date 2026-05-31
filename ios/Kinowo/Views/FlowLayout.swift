import SwiftUI
import CoreGraphics

struct FlowLayout: Layout {
    var spacing: CGFloat = 4
    var lineSpacing: CGFloat = 4
    var justified: Bool = false
    var centered: Bool = false
    /// Align each item to the bottom of its line rather than the top.
    /// Items of differing height (e.g. a larger plain-text year next to
    /// smaller pills) then share a common bottom edge, so their text
    /// bottoms line up.
    var bottomAligned: Bool = false

    struct Cache { var size: CGSize = .zero }

    func makeCache(subviews: Subviews) -> Cache { Cache() }
    func updateCache(_ cache: inout Cache, subviews: Subviews) { cache.size = .zero }

    func sizeThatFits(proposal: ProposedViewSize, subviews: Subviews, cache: inout Cache) -> CGSize {
        let sizes = subviews.map { $0.sizeThatFits(.unspecified) }
        return FlowLayoutMath.layout(
            sizes: sizes,
            maxWidth: proposal.width ?? .infinity,
            spacing: spacing,
            lineSpacing: lineSpacing,
            justified: justified,
            centered: centered
        ).totalSize
    }

    func placeSubviews(in bounds: CGRect, proposal: ProposedViewSize, subviews: Subviews, cache: inout Cache) {
        let sizes = subviews.map { $0.sizeThatFits(.unspecified) }
        let result = FlowLayoutMath.layout(
            sizes: sizes,
            maxWidth: bounds.width,
            spacing: spacing,
            lineSpacing: lineSpacing,
            justified: justified,
            centered: centered
        )
        // Per-line height, keyed by the line's top y (shared by every
        // item placed on that line). Only needed to push shorter items
        // down to a common bottom edge when `bottomAligned`.
        var lineHeightByTop: [CGFloat: CGFloat] = [:]
        if bottomAligned {
            for (i, s) in sizes.enumerated() {
                let top = result.positions[i].y
                lineHeightByTop[top] = max(lineHeightByTop[top] ?? 0, s.height.rounded(.up))
            }
        }
        for (i, sub) in subviews.enumerated() {
            let p: ProposedViewSize = justified
                ? ProposedViewSize(width: result.proposedWidths[i], height: nil)
                : .unspecified
            let top = result.positions[i].y
            let dy = bottomAligned
                ? (lineHeightByTop[top] ?? 0) - sizes[i].height.rounded(.up)
                : 0
            sub.place(
                at: CGPoint(x: bounds.minX + result.positions[i].x, y: bounds.minY + top + dy),
                anchor: .topLeading,
                proposal: p
            )
        }
    }
}

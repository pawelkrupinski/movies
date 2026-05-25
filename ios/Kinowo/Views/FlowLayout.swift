import SwiftUI
import CoreGraphics

struct FlowLayout: Layout {
    var spacing: CGFloat = 4
    var lineSpacing: CGFloat = 4
    var justified: Bool = false
    var centered: Bool = false

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
        for (i, sub) in subviews.enumerated() {
            let p: ProposedViewSize = justified
                ? ProposedViewSize(width: result.proposedWidths[i], height: nil)
                : .unspecified
            sub.place(
                at: CGPoint(x: bounds.minX + result.positions[i].x, y: bounds.minY + result.positions[i].y),
                anchor: .topLeading,
                proposal: p
            )
        }
    }
}

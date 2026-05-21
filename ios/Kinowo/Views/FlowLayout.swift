import SwiftUI

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
        let maxWidth = proposal.width ?? .infinity
        var x: CGFloat = 0
        var y: CGFloat = 0
        var lineHeight: CGFloat = 0
        var maxLineWidth: CGFloat = 0
        for sub in subviews {
            let s = sub.sizeThatFits(.unspecified)
            if x > 0, x + s.width > maxWidth {
                maxLineWidth = max(maxLineWidth, x - spacing)
                x = 0
                y += lineHeight + lineSpacing
                lineHeight = 0
            }
            x += s.width + spacing
            lineHeight = max(lineHeight, s.height)
        }
        maxLineWidth = max(maxLineWidth, x - spacing)
        let totalHeight = y + lineHeight
        let width = maxWidth.isFinite ? min(maxWidth, max(maxLineWidth, 0)) : max(maxLineWidth, 0)
        return CGSize(width: max(0, width), height: max(0, totalHeight))
    }

    func placeSubviews(in bounds: CGRect, proposal: ProposedViewSize, subviews: Subviews, cache: inout Cache) {
        let maxWidth = bounds.width
        var x: CGFloat = bounds.minX
        var y: CGFloat = bounds.minY
        var lineHeight: CGFloat = 0
        for sub in subviews {
            let s = sub.sizeThatFits(.unspecified)
            if x > bounds.minX, x + s.width > bounds.minX + maxWidth {
                x = bounds.minX
                y += lineHeight + lineSpacing
                lineHeight = 0
            }
            sub.place(at: CGPoint(x: x, y: y),
                      anchor: .topLeading,
                      proposal: ProposedViewSize(width: s.width, height: s.height))
            x += s.width + spacing
            lineHeight = max(lineHeight, s.height)
        }
    }
}

#if canImport(CoreGraphics)
import CoreGraphics

enum FlowLayoutMath {
    struct Result {
        let positions: [CGPoint]
        let totalSize: CGSize
        let proposedWidths: [CGFloat]
    }

    static func layout(
        sizes: [CGSize],
        maxWidth: CGFloat,
        spacing: CGFloat,
        lineSpacing: CGFloat,
        justified: Bool = false,
        centered: Bool = false
    ) -> Result {
        var positions: [CGPoint] = []
        positions.reserveCapacity(sizes.count)
        var proposedWidths: [CGFloat] = []
        proposedWidths.reserveCapacity(sizes.count)
        var x: CGFloat = 0
        var y: CGFloat = 0
        var lineHeight: CGFloat = 0
        var maxLineWidth: CGFloat = 0
        var lineStart = 0

        func finishLine(end: Int) {
            guard maxWidth.isFinite, end > lineStart else { return }
            let count = end - lineStart
            let naturalWidth = (lineStart..<end).reduce(CGFloat(0)) { acc, i in
                acc + sizes[i].width.rounded(.up)
            } + spacing * CGFloat(count - 1)
            let extra = maxWidth - naturalWidth
            guard extra > 0 else { return }

            if justified {
                let perItem = extra / CGFloat(count)
                var newX: CGFloat = 0
                for i in lineStart..<end {
                    positions[i] = CGPoint(x: newX, y: positions[i].y)
                    let w = sizes[i].width.rounded(.up) + perItem
                    proposedWidths[i] = w
                    newX += w + spacing
                }
            } else if centered {
                let offset = extra / 2
                for i in lineStart..<end {
                    positions[i] = CGPoint(x: positions[i].x + offset, y: positions[i].y)
                }
            }
        }

        for (i, s) in sizes.enumerated() {
            let w = s.width.rounded(.up)
            let h = s.height.rounded(.up)
            if x > 0, x + w > maxWidth {
                finishLine(end: i)
                maxLineWidth = max(maxLineWidth, x - spacing)
                lineStart = i
                x = 0
                y += lineHeight + lineSpacing
                lineHeight = 0
            }
            positions.append(CGPoint(x: x, y: y))
            proposedWidths.append(w)
            x += w + spacing
            lineHeight = max(lineHeight, h)
        }
        finishLine(end: sizes.count)
        maxLineWidth = max(maxLineWidth, x - spacing)
        let totalHeight = y + lineHeight
        let width = maxWidth.isFinite ? min(maxWidth, max(maxLineWidth, 0)) : max(maxLineWidth, 0)
        return Result(
            positions: positions,
            totalSize: CGSize(width: max(0, width), height: max(0, totalHeight)),
            proposedWidths: proposedWidths
        )
    }
}
#endif

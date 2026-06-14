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
        var xPosition: CGFloat = 0
        var yPosition: CGFloat = 0
        var lineHeight: CGFloat = 0
        var maxLineWidth: CGFloat = 0
        var lineStart = 0

        func finishLine(end: Int) {
            guard maxWidth.isFinite, end > lineStart else { return }
            let count = end - lineStart
            let naturalWidth = (lineStart..<end).reduce(CGFloat(0)) { accumulator, i in
                accumulator + sizes[i].width.rounded(.up)
            } + spacing * CGFloat(count - 1)
            let extra = maxWidth - naturalWidth
            guard extra > 0 else { return }

            if justified {
                let perItem = extra / CGFloat(count)
                var newXPosition: CGFloat = 0
                for i in lineStart..<end {
                    positions[i] = CGPoint(x: newXPosition, y: positions[i].y)
                    let width = sizes[i].width.rounded(.up) + perItem
                    proposedWidths[i] = width
                    newXPosition += width + spacing
                }
            } else if centered {
                let offset = extra / 2
                for i in lineStart..<end {
                    positions[i] = CGPoint(x: positions[i].x + offset, y: positions[i].y)
                }
            }
        }

        for (i, size) in sizes.enumerated() {
            let width = size.width.rounded(.up)
            let height = size.height.rounded(.up)
            if xPosition > 0, xPosition + width > maxWidth {
                finishLine(end: i)
                maxLineWidth = max(maxLineWidth, xPosition - spacing)
                lineStart = i
                xPosition = 0
                yPosition += lineHeight + lineSpacing
                lineHeight = 0
            }
            positions.append(CGPoint(x: xPosition, y: yPosition))
            proposedWidths.append(width)
            xPosition += width + spacing
            lineHeight = max(lineHeight, height)
        }
        finishLine(end: sizes.count)
        maxLineWidth = max(maxLineWidth, xPosition - spacing)
        let totalHeight = yPosition + lineHeight
        let width = maxWidth.isFinite ? min(maxWidth, max(maxLineWidth, 0)) : max(maxLineWidth, 0)
        return Result(
            positions: positions,
            totalSize: CGSize(width: max(0, width), height: max(0, totalHeight)),
            proposedWidths: proposedWidths
        )
    }
}
#endif

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
        var flow = FlowLines(sizes: sizes, maxWidth: maxWidth, spacing: spacing,
                             justified: justified, centered: centered)
        var yPosition: CGFloat = 0
        var lineHeight: CGFloat = 0
        var maxLineWidth: CGFloat = 0
        var xPosition: CGFloat = 0

        for (i, size) in sizes.enumerated() {
            let width = size.width.rounded(.up)
            let height = size.height.rounded(.up)
            if xPosition > 0, xPosition + width > maxWidth {
                flow.finishLine(end: i)
                maxLineWidth = max(maxLineWidth, xPosition - spacing)
                flow.lineStart = i
                xPosition = 0
                yPosition += lineHeight + lineSpacing
                lineHeight = 0
            }
            flow.positions.append(CGPoint(x: xPosition, y: yPosition))
            flow.proposedWidths.append(width)
            xPosition += width + spacing
            lineHeight = max(lineHeight, height)
        }
        flow.finishLine(end: sizes.count)
        maxLineWidth = max(maxLineWidth, xPosition - spacing)
        let totalHeight = yPosition + lineHeight
        let width = maxWidth.isFinite ? min(maxWidth, max(maxLineWidth, 0)) : max(maxLineWidth, 0)
        return Result(
            positions: flow.positions,
            totalSize: CGSize(width: max(0, width), height: max(0, totalHeight)),
            proposedWidths: flow.proposedWidths
        )
    }
}

/// The items placed so far, and the justify / centre pass over a finished line.
private struct FlowLines {
    let sizes: [CGSize]
    let maxWidth: CGFloat
    let spacing: CGFloat
    let justified: Bool
    let centered: Bool
    var positions: [CGPoint] = []
    var proposedWidths: [CGFloat] = []
    var lineStart = 0

    init(sizes: [CGSize], maxWidth: CGFloat, spacing: CGFloat, justified: Bool, centered: Bool) {
        self.sizes = sizes
        self.maxWidth = maxWidth
        self.spacing = spacing
        self.justified = justified
        self.centered = centered
        positions.reserveCapacity(sizes.count)
        proposedWidths.reserveCapacity(sizes.count)
    }

    mutating func finishLine(end: Int) {
        guard maxWidth.isFinite, end > lineStart else { return }
        let count = end - lineStart
        let naturalWidth = (lineStart..<end).reduce(CGFloat(0)) { accumulator, i in
            accumulator + sizes[i].width.rounded(.up)
        } + spacing * CGFloat(count - 1)
        let extra = maxWidth - naturalWidth
        guard extra > 0 else { return }

        if justified {
            justify(end: end, perItem: extra / CGFloat(count))
        } else if centered {
            let offset = extra / 2
            for i in lineStart..<end {
                positions[i] = CGPoint(x: positions[i].x + offset, y: positions[i].y)
            }
        }
    }

    private mutating func justify(end: Int, perItem: CGFloat) {
        var newXPosition: CGFloat = 0
        for i in lineStart..<end {
            positions[i] = CGPoint(x: newXPosition, y: positions[i].y)
            let width = sizes[i].width.rounded(.up) + perItem
            proposedWidths[i] = width
            newXPosition += width + spacing
        }
    }
}
#endif

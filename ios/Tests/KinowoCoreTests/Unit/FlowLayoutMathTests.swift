import XCTest
import CoreGraphics
@testable import KinowoCore

final class FlowLayoutMathTests: XCTestCase {

    func testThreeSmallPillsStayOnLineOne() {
        let sizes = [
            CGSize(width: 50, height: 16),
            CGSize(width: 22, height: 16),
            CGSize(width: 44, height: 16),
        ]
        let r = FlowLayoutMath.layout(sizes: sizes, maxWidth: 154, spacing: 4, lineSpacing: 4)
        XCTAssertTrue(r.positions.allSatisfy { $0.y == 0 }, "expected all pills on line 1, got \(r.positions)")
        XCTAssertEqual(r.totalSize.height, 16)
    }

    func testFractionalWidthPillsCeilingFitWithinMaxWidth() {
        // Reproduces the Mandalorian + Grogu rounding regression: each
        // pill comes back at x.3 pt and the sum + spacing should still
        // fit when widths are rounded UP before the wrap check.
        let sizes = [
            CGSize(width: 50.3, height: 16.4),
            CGSize(width: 22.1, height: 16.4),
            CGSize(width: 44.2, height: 16.4),
        ]
        let r = FlowLayoutMath.layout(sizes: sizes, maxWidth: 130, spacing: 4, lineSpacing: 4)
        XCTAssertTrue(r.positions.allSatisfy { $0.y == 0 }, "fractional pills should not wrap; got \(r.positions)")
    }

    func testFourthPillWrapsWhenItWouldOverflow() {
        let sizes = [
            CGSize(width: 50, height: 16),
            CGSize(width: 22, height: 16),
            CGSize(width: 44, height: 16),
            CGSize(width: 50, height: 16),
        ]
        let r = FlowLayoutMath.layout(sizes: sizes, maxWidth: 150, spacing: 4, lineSpacing: 4)
        XCTAssertEqual(r.positions[0].y, 0)
        XCTAssertEqual(r.positions[1].y, 0)
        XCTAssertEqual(r.positions[2].y, 0)
        XCTAssertGreaterThan(r.positions[3].y, 0, "4th pill should wrap, got y=\(r.positions[3].y)")
    }

    func testSingleOverlongPillStaysOnLineOneAndOverflows() {
        let sizes = [CGSize(width: 300, height: 16)]
        let r = FlowLayoutMath.layout(sizes: sizes, maxWidth: 100, spacing: 4, lineSpacing: 4)
        XCTAssertEqual(r.positions[0], .zero, "an oversized lone pill must land at the origin")
    }

    func testEmptyInputReturnsZeroSize() {
        let r = FlowLayoutMath.layout(sizes: [], maxWidth: 100, spacing: 4, lineSpacing: 4)
        XCTAssertTrue(r.positions.isEmpty)
        XCTAssertEqual(r.totalSize, .zero)
    }

    func testCeilingRoundingMakesThreeRoundedPillsFitExactBound() {
        let sizes = Array(repeating: CGSize(width: 50.6, height: 16), count: 3)
        let fits  = FlowLayoutMath.layout(sizes: sizes, maxWidth: 161, spacing: 4, lineSpacing: 4)
        let wraps = FlowLayoutMath.layout(sizes: sizes, maxWidth: 160, spacing: 4, lineSpacing: 4)
        XCTAssertTrue(fits.positions.allSatisfy { $0.y == 0 }, "expected fit at 161 pt, got \(fits.positions)")
        XCTAssertGreaterThan(wraps.positions[2].y, 0, "expected wrap at 160 pt, got \(wraps.positions)")
    }

    func testTotalSizeWidthStaysWithinMaxWidthForFiniteBound() {
        let sizes = [
            CGSize(width: 50, height: 16),
            CGSize(width: 22, height: 16),
            CGSize(width: 44, height: 16),
            CGSize(width: 50, height: 16),
        ]
        let r = FlowLayoutMath.layout(sizes: sizes, maxWidth: 150, spacing: 4, lineSpacing: 4)
        XCTAssertLessThanOrEqual(r.totalSize.width, 150)
    }
}

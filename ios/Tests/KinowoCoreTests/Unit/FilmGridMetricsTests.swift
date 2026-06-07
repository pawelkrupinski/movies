import XCTest
#if canImport(CoreGraphics)
import CoreGraphics
@testable import KinowoCore

/// The repertoire grid's column count must be a pure function of the
/// window width — portrait is always two columns, landscape grows wider.
/// The on-device bug this guards: rotating to landscape and back to portrait
/// sometimes left the grid stuck at the landscape column count ("zoomed-in",
/// more than two columns in portrait), because `GridItem(.adaptive:)` read a
/// stale scroll-view width from the paged `TabView`. `FilmGridView` now drives
/// the count off this formula and the authoritative window width instead.
final class FilmGridMetricsTests: XCTestCase {

    /// Every supported portrait iPhone width must yield exactly two columns —
    /// the floor the two-pills-per-row card guarantee is also tuned against
    /// (see `ShowtimePillMetricsTests`). This is the regression assertion: a
    /// rotation must never leave portrait showing more than two columns.
    func testPortraitWidthsYieldTwoColumns() {
        let portrait: [(name: String, width: CGFloat)] = [
            ("13 / 14 / 16 (floor)", 390),
            ("15 / 16 Pro", 393),
            ("16 / 17", 402),
            ("14 Plus / Pro Max", 430),
            ("16 / 17 Pro Max", 440),
        ]
        for phone in portrait {
            XCTAssertEqual(
                FilmGridMetrics.columnCount(forWidth: phone.width), 2,
                "\(phone.name) (\(phone.width) pt portrait) must be two columns")
        }
    }

    /// Landscape (the portrait device's longer edge) earns the extra columns —
    /// the point of rotating. Four on the mainstream phones, five-plus on the
    /// Pro Max sizes. Mirrors what `.adaptive(minimum: 160)` chose before, so
    /// the visible landscape layout is unchanged — only the portrait staleness
    /// is fixed.
    func testLandscapeWidthsYieldMoreThanTwoColumns() {
        XCTAssertEqual(FilmGridMetrics.columnCount(forWidth: 844), 4, "14 / 15 landscape")
        XCTAssertEqual(FilmGridMetrics.columnCount(forWidth: 852), 4, "14 / 15 Plus landscape")
        XCTAssertGreaterThanOrEqual(
            FilmGridMetrics.columnCount(forWidth: 932), 5, "Pro Max landscape")
    }

    /// Widening the window never drops a column — the count is a floor of a
    /// monotonic ratio, so there can be no width at which rotating *wider*
    /// shows *fewer* columns.
    func testColumnCountIsMonotonicInWidth() {
        var last = 0
        for w in stride(from: CGFloat(300), through: 1400, by: 1) {
            let n = FilmGridMetrics.columnCount(forWidth: w)
            XCTAssertGreaterThanOrEqual(
                n, last, "column count must not drop as width grows (at \(w) pt)")
            last = n
        }
    }

    /// Degenerate widths still lay out a single column rather than zero —
    /// a LazyVGrid with no columns renders nothing.
    func testNeverFewerThanOneColumn() {
        XCTAssertEqual(FilmGridMetrics.columnCount(forWidth: 0), 1)
        XCTAssertEqual(FilmGridMetrics.columnCount(forWidth: 50), 1)
        XCTAssertEqual(FilmGridMetrics.columnCount(forWidth: -100), 1)
    }
}
#endif

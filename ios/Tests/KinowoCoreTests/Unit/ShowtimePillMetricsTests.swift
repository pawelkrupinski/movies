import XCTest
#if canImport(CoreText)
import CoreGraphics
@testable import KinowoCore

final class ShowtimePillMetricsTests: XCTestCase {

    /// Content width of one card's showings column on the narrowest
    /// two-column portrait phone we support (iPhone SE / 13 mini at
    /// 375 pt): the grid's 12 pt horizontal padding + 12 pt inter-column
    /// spacing give a card of `(375 - 36) / 2`, and `FilmCardView`'s
    /// `.padding(12)` then takes 24 pt off the inside. Every wider phone
    /// (incl. the Pro Max) gives the `FlowLayout` more room, so a fit
    /// here is a fit everywhere in portrait.
    private let narrowestPortraitContent: CGFloat = (375 - 36) / 2 - 24

    func testTwoCanonicalPillsShareOneRowInNarrowestPortraitCard() {
        let a = ShowtimePillMetrics.pillWidth(time: "12:55", format: "2D DUB")
        let b = ShowtimePillMetrics.pillWidth(time: "22:55", format: "3D NAP")

        let result = FlowLayoutMath.layout(
            sizes: [CGSize(width: a, height: 16), CGSize(width: b, height: 16)],
            maxWidth: narrowestPortraitContent,
            spacing: ShowtimePillMetrics.interPillGap,
            lineSpacing: 4
        )

        XCTAssertTrue(
            result.positions.allSatisfy { $0.y == 0 },
            "two showtime pills must share one row at \(narrowestPortraitContent) pt "
            + "(widths a=\(a), b=\(b)); got \(result.positions)"
        )
    }

    func testFormatlessPillIsNarrowerThanFormattedOne() {
        let bare = ShowtimePillMetrics.pillWidth(time: "18:00", format: "")
        let tagged = ShowtimePillMetrics.pillWidth(time: "18:00", format: "2D DUB")
        XCTAssertLessThan(bare, tagged, "a pill with no format tag must be narrower")
    }
}
#endif

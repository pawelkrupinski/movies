import XCTest
#if canImport(CoreText)
import CoreGraphics
@testable import KinowoCore

final class ShowtimePillMetricsTests: XCTestCase {

    /// At the 11 pt time font the chips no longer fit two per row on a
    /// portrait card — that earlier guarantee was deliberately traded for
    /// legibility (matching Android's 11sp chip) — so the contract this pins
    /// is narrower: a single canonical pill (time + a format token) still
    /// fits inside the narrowest card's showings column without overflowing.
    func testASinglePillFitsTheNarrowestCard() {
        let worstCase = ShowtimePillMetrics.pillWidth(time: "22:55", format: "3D NAP")
        let content = ShowtimePillMetrics.cardShowingsWidth(screenWidth: 375)

        XCTAssertGreaterThan(worstCase, 0, "pill measured no width — metrics are stubbed")
        XCTAssertLessThanOrEqual(
            worstCase, content,
            "a single showtime pill overflowed the \(content) pt card "
            + "(375 pt phone); pill width \(worstCase)"
        )
    }

    /// The inverse guard: two canonical pills are now wide enough that
    /// `FlowLayout` wraps the second onto a new row in the narrowest card.
    /// Proves the width math is real — a stub zero-width renderer would keep
    /// them on one row and fail this — and documents the trade made above.
    func testTwoPillsWrapInTheNarrowestCard() {
        let a = ShowtimePillMetrics.pillWidth(time: "12:55", format: "2D DUB")
        let b = ShowtimePillMetrics.pillWidth(time: "22:55", format: "3D NAP")
        let content = ShowtimePillMetrics.cardShowingsWidth(screenWidth: 375)
        let result = FlowLayoutMath.layout(
            sizes: [CGSize(width: a, height: 16), CGSize(width: b, height: 16)],
            maxWidth: content,
            spacing: ShowtimePillMetrics.interPillGap,
            lineSpacing: 4
        )
        XCTAssertEqual(
            result.positions.count, 2, "expected both pills laid out; got \(result.positions)")
        XCTAssertGreaterThan(
            result.positions[1].y, result.positions[0].y,
            "two canonical pills should wrap onto separate rows in the "
            + "\(content) pt card; got \(result.positions)"
        )
    }

    /// Pins the chip font sizes the on-screen `ShowtimeBadge` renders at:
    /// 11 pt time / 9 pt format, bumped up for arm's-length legibility to
    /// match Android's 11sp/9sp chip.
    func testFontSizesAreElevenAndNine() {
        XCTAssertEqual(ShowtimePillMetrics.timeFontSize, 11, accuracy: 0.001)
        XCTAssertEqual(ShowtimePillMetrics.formatFontSize, 9, accuracy: 0.001)
    }

    func testFormatlessPillIsNarrowerThanFormattedOne() {
        let bare = ShowtimePillMetrics.pillWidth(time: "18:00", format: "")
        let tagged = ShowtimePillMetrics.pillWidth(time: "18:00", format: "2D DUB")
        XCTAssertLessThan(bare, tagged, "a pill with no format tag must be narrower")
    }

    /// The pill colours `ShowtimeBadge` renders from must match the web
    /// `.badge-time` palette: `#3a3a6e` fill, `#5a5a9e` pressed/hover, `#aad4ff`
    /// time text, format tag at 0.7 alpha. (`ShowtimeBadge` lives in the
    /// Xcode-only app target, so the on-screen render is pixel-verified on
    /// Android's twin test; here we pin the shared source the view builds from.)
    func testPillColoursMatchTheWebBadgePalette() {
        func assertRGB(_ got: (red: Double, green: Double, blue: Double),
                       _ hex: Int, _ label: String) {
            let r = Double((hex >> 16) & 0xFF) / 255
            let g = Double((hex >> 8) & 0xFF) / 255
            let b = Double(hex & 0xFF) / 255
            XCTAssertEqual(got.red, r, accuracy: 0.5 / 255, "\(label) red")
            XCTAssertEqual(got.green, g, accuracy: 0.5 / 255, "\(label) green")
            XCTAssertEqual(got.blue, b, accuracy: 0.5 / 255, "\(label) blue")
        }
        assertRGB(ShowtimePillMetrics.backgroundRGB, 0x3A3A6E, "fill")
        assertRGB(ShowtimePillMetrics.pressedBackgroundRGB, 0x5A5A9E, "pressed fill")
        assertRGB(ShowtimePillMetrics.textRGB, 0xAAD4FF, "time text")
        XCTAssertEqual(ShowtimePillMetrics.formatAlpha, 0.7, accuracy: 0.001, "format tag alpha")
    }

    /// The pill's vertical inset must match the web mobile `.badge-time` rule
    /// (`padding: .2em`), i.e. 0.2 × the time font — so the pill reads the same
    /// height as the web's, not the old flat 4 pt (twice as tall). (`ShowtimeBadge`
    /// is in the Xcode-only app target; this pins the inset the view reads from.)
    func testPillVerticalInsetMatchesTheWebMobilePadding() {
        XCTAssertEqual(
            ShowtimePillMetrics.verticalInset,
            ShowtimePillMetrics.timeFontSize * 0.2,
            accuracy: 0.5,
            "vertical inset should be the web's .2em (0.2 × time font)")
    }
}
#endif

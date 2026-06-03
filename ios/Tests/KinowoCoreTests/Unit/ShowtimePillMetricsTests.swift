import XCTest
#if canImport(CoreText)
import CoreGraphics
@testable import KinowoCore

final class ShowtimePillMetricsTests: XCTestCase {

    /// Logical point widths of every iPhone on our iOS 16 deployment
    /// floor, narrowest first. The 375 pt phones (SE 3, 13 mini) are the
    /// binding case; the rest must keep fitting as screens grow. Derived
    /// from `ShowtimePillMetrics.cardShowingsWidth` — the same formula the
    /// grid uses — so a change to the column layout moves the test with it.
    private static let iPhoneWidths: [(name: String, width: CGFloat)] = [
        ("SE 3 / 13 mini", 375),
        ("13 / 14 / 16",   390),
        ("15 / 16 Pro",    393),
        ("16 / 17",        402),
        ("14 Plus / Pro Max", 430),
        ("16 / 17 Pro Max", 440),
    ]

    /// Two canonical pills MUST share one row in every portrait card. This is
    /// a hard, inviolable constraint — fonts are sized up to the largest that
    /// keeps it (currently 11 pt time / 7 pt format), never past it. If this
    /// fails, a font/padding/gap change broke two-per-row; shrink it back or
    /// reclaim width (trim inset/gap), don't relax this assertion.
    func testTwoCanonicalPillsShareOneRowInEveryPortraitResolution() {
        let a = ShowtimePillMetrics.pillWidth(time: "12:55", format: "2D DUB")
        let b = ShowtimePillMetrics.pillWidth(time: "22:55", format: "3D NAP")

        for phone in Self.iPhoneWidths {
            let content = ShowtimePillMetrics.cardShowingsWidth(screenWidth: phone.width)
            let result = FlowLayoutMath.layout(
                sizes: [CGSize(width: a, height: 16), CGSize(width: b, height: 16)],
                maxWidth: content,
                spacing: ShowtimePillMetrics.interPillGap,
                lineSpacing: 4
            )
            XCTAssertTrue(
                result.positions.allSatisfy { $0.y == 0 },
                "two showtime pills must share one row on \(phone.name) "
                + "(\(phone.width) pt → \(content) pt card; widths a=\(a), b=\(b)); "
                + "got \(result.positions)"
            )
        }
    }

    /// Pins the chip font sizes the on-screen `ShowtimeBadge` renders at:
    /// 11 pt time / 7 pt format. The tag is held at 7 (not 8) so the pill can
    /// carry the roomier uniform 4 pt inset and still fit two-up — see the
    /// two-per-row test above; don't grow either without re-measuring.
    func testFontSizesAreElevenAndSeven() {
        XCTAssertEqual(ShowtimePillMetrics.timeFontSize, 11, accuracy: 0.001)
        XCTAssertEqual(ShowtimePillMetrics.formatFontSize, 7, accuracy: 0.001)
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

    /// The pill inset is uniform — the same on both axes — so the time sits in
    /// an even box of padding rather than a wide-short one. (`ShowtimeBadge` is
    /// in the Xcode-only app target; this pins the insets the view reads from.)
    func testPillInsetIsUniform() {
        XCTAssertEqual(
            ShowtimePillMetrics.verticalInset,
            ShowtimePillMetrics.horizontalInset,
            accuracy: 0.001,
            "pill inset should be uniform on both axes")
    }
}
#endif

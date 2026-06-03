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

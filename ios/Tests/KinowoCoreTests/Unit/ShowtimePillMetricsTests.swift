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
}
#endif

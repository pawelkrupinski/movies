import XCTest
@testable import KinowoCore

/// Pins the rating-pill font sizes `RatingBadgesView` renders from. The view
/// itself is excluded from `KinowoCore` (SwiftUI), so its on-screen render is
/// pixel-verified by Android's twin; here we lock the shared source constants.
final class RatingBadgeMetricsTests: XCTestCase {

    /// The score value (and Metacritic's label-less solid pill) reads at
    /// 11 pt, matching the showtime chip's time and Android's 11sp pill —
    /// bumped up from the old 10 pt for legibility.
    func testValueFontIsEleven() {
        XCTAssertEqual(RatingBadgeMetrics.valueFontSize, 11, accuracy: 0.001)
    }

    /// The colored label tab stays one point below the value so it reads as a
    /// subordinate tag rather than competing with the score.
    func testLabelFontIsOnePointBelowTheValue() {
        XCTAssertEqual(
            RatingBadgeMetrics.labelFontSize,
            RatingBadgeMetrics.valueFontSize - 1,
            accuracy: 0.001
        )
    }
}

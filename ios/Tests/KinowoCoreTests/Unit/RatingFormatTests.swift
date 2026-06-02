import XCTest
@testable import KinowoCore

/// Locks the IMDb / Filmweb pill score format (`Film.Ratings.scoreText`). The
/// bug this guards: a whole-number score (7.0) must render "7.0", not "7", to
/// stay in step with web + Android.
final class RatingFormatTests: XCTestCase {

    func testWholeNumberKeepsTenthsPlace() {
        XCTAssertEqual(Film.Ratings.scoreText(7.0), "7.0")
        XCTAssertEqual(Film.Ratings.scoreText(8.0), "8.0")
    }

    func testFractionalShownToOneDecimal() {
        XCTAssertEqual(Film.Ratings.scoreText(7.7), "7.7")
    }

    func testRawScoreRoundedToOneDecimal() {
        // A real filmweb value: rounds to 7.0 and must still show the ".0".
        XCTAssertEqual(Film.Ratings.scoreText(6.97571), "7.0")
        XCTAssertEqual(Film.Ratings.scoreText(7.84), "7.8")
    }
}

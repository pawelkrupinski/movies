import XCTest
@testable import KinowoCore

/// Regression for the day header staying in the DEPLOYMENT's default
/// language after a visitor picks a different in-app language
/// (`LanguageSelection`, independent of the deployment/country) — the
/// server's `DayShowings.label` is baked once and never reflects that pick,
/// so `ShowingsView` derives the label from `DateLabel.format` instead.
/// Mirrors `DateFormatter.scala`'s output byte-for-byte for the languages
/// this app ships.
final class DateLabelTests: XCTestCase {

    func testPolishUsesGenitiveMonthAndCapitalizedWeekday() {
        // 2026-06-04 is a Thursday.
        XCTAssertEqual(
            DateLabel.format(isoDate: "2026-06-04", locale: Locale(identifier: "pl")),
            "Czwartek 4 czerwca"
        )
    }

    func testEnglishUsesTheDeviceLocaleFullNames() {
        XCTAssertEqual(
            DateLabel.format(isoDate: "2026-06-04", locale: Locale(identifier: "en")),
            "Thursday 4 June"
        )
    }

    func testGermanUsesItsOwnFullNames() {
        XCTAssertEqual(
            DateLabel.format(isoDate: "2026-06-04", locale: Locale(identifier: "de")),
            "Donnerstag 4 Juni"
        )
    }

    func testSpanishUsesItsOwnFullNames() {
        XCTAssertEqual(
            DateLabel.format(isoDate: "2026-06-04", locale: Locale(identifier: "es")),
            "Jueves 4 junio" // weekday capitalized (matches DateFormatter.scala), month stays lowercase
        )
    }

    func testAppendsTheYearOnlyWhenItDiffersFromTheCurrentOne() {
        let currentYear = Calendar(identifier: .gregorian).component(.year, from: Date())
        let sameYearLabel = DateLabel.format(isoDate: "\(currentYear)-06-04", locale: Locale(identifier: "en"))
        XCTAssertFalse(sameYearLabel.contains("\(currentYear)"))

        let otherYear = currentYear + 1
        let otherYearLabel = DateLabel.format(isoDate: "\(otherYear)-06-04", locale: Locale(identifier: "en"))
        XCTAssertTrue(otherYearLabel.hasSuffix(" \(otherYear)"))
    }

    func testTheSameDateSwitchesLanguageWhenTheLocaleChanges() {
        // The exact bug this fixes: picking a different in-app language must
        // change this text, not leave it stuck in whatever the deployment
        // (or a previous pick) rendered.
        let polish  = DateLabel.format(isoDate: "2026-06-04", locale: Locale(identifier: "pl"))
        let english = DateLabel.format(isoDate: "2026-06-04", locale: Locale(identifier: "en"))
        XCTAssertNotEqual(polish, english)
    }

    /// The doc promises an unparseable date comes back as-is. `Calendar`
    /// silently rolls an out-of-range month/day over instead of failing, so
    /// month 13 used to index past the month tables and trap.
    func testOutOfRangeDateComesBackUnchangedInsteadOfCrashing() {
        XCTAssertEqual(DateLabel.format(isoDate: "2026-13-04", locale: Locale(identifier: "pl")), "2026-13-04")
        XCTAssertEqual(DateLabel.format(isoDate: "2026-13-04", locale: Locale(identifier: "en")), "2026-13-04")
        XCTAssertEqual(DateLabel.format(isoDate: "2026-02-30", locale: Locale(identifier: "en")), "2026-02-30")
    }
}

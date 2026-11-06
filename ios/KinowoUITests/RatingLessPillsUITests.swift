import XCTest

/// A source we hold a link for but no score yet (a new release) still gets its
/// IMDb / Filmweb pill: the source's short name in the score slot and an accessible
/// "no rating yet" label, translated per app language. Driven by the
/// `KINOWO_UITEST_PENDING_RATINGS` fixture: every card carries an IMDb page and a
/// Filmweb page and no scores, so the pills can only come from the rating-less path.
@MainActor
final class RatingLessPillsUITests: XCTestCase {

    private func launch(language: String) -> XCUIApplication {
        continueAfterFailure = false
        let app = XCUIApplication()
        FixtureLaunch.intoGrid(app, language: language, environment: ["KINOWO_UITEST_PENDING_RATINGS": "1"])
        return app
    }

    private func assertPills(language: String, imdb: String, filmweb: String) {
        let app = launch(language: language)
        XCTAssertTrue(FixtureLaunch.firstFilmCard(app).waitForExistence(timeout: 30), "Grid never appeared")
        for label in [imdb, filmweb] {
            let pill = app.descendants(matching: .any).matching(NSPredicate(format: "label == %@", label)).firstMatch
            XCTAssertTrue(pill.waitForExistence(timeout: 15), "No rating-less pill labelled \"\(label)\" in \(language)")
            XCTAssertTrue(pill.frame.width > 0)
        }
    }

    func testPolish() { assertPills(language: "pl", imdb: "IMDb – brak oceny", filmweb: "Filmweb – brak oceny") }
    func testGerman() { assertPills(language: "de", imdb: "IMDb – noch keine Bewertung", filmweb: "Filmweb – noch keine Bewertung") }
    func testEnglish() { assertPills(language: "en", imdb: "IMDb, no rating yet", filmweb: "Filmweb, no rating yet") }
    func testSpanish() { assertPills(language: "es", imdb: "IMDb – sin valoración todavía", filmweb: "Filmweb – sin valoración todavía") }
}

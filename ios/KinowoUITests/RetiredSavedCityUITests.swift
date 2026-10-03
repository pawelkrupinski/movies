import XCTest

/// A city saved before its page was retired — `miedzyrzec-podlaski`, now part
/// of `biala-podlaska` — is adopted under the slug it answers at. Its listing
/// kept arriving through the server's 301, but the Filtry sheet found no city
/// by the old slug and labelled the list with the raw slug instead.
final class RetiredSavedCityUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
    }

    override func tearDownWithError() throws { app = nil }

    func testASavedRetiredCityIsShownAsTheCityThatHoldsItNow() throws {
        FixtureLaunch.intoGrid(app, city: "miedzyrzec-podlaski", environment: [
            "KINOWO_UITEST_OPEN_FILTERS": "1",
            // The bundled seed, which names the retired slug on its successor —
            // not whatever catalog an earlier run persisted on the simulator.
            "KINOWO_CLEAR_CATALOG_CACHE": "1",
        ])

        let label = app.staticTexts.containing(NSPredicate(format: "label BEGINSWITH %@", "Biała Podlaska i okolice")).firstMatch
        for _ in 0..<8 where !label.exists { app.swipeUp() }
        XCTAssertTrue(label.waitForExistence(timeout: 30),
                      "Filtry should name the city the retired slug answers at now")
        XCTAssertFalse(app.staticTexts.containing(NSPredicate(format: "label CONTAINS %@", "miedzyrzec-podlaski")).firstMatch.exists,
                       "Filtry still labels the list with the retired slug")
    }
}

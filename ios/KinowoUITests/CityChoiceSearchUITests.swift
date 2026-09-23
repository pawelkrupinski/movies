import XCTest

/// The manual city picker (`CityChoiceView`) carries a search field that
/// narrows the list as you type — diacritic-insensitive, so "wroc" surfaces
/// "Wrocław" and drops everything else. Poland opens on its 16 voivodeships,
/// so the query also flattens the groups to reach a city inside one.
///
/// Reaches the picker deterministically: `KINOWO_CLEAR_CITY` shows the gate,
/// `KINOWO_FORCE_DETECTED_CITY` injects a detected city (no CoreLocation
/// dialog) so the confirm screen appears, then we tap "choose another city"
/// to drop into the manual list.
final class CityChoiceSearchUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
        FixtureLaunch.pinCountryAndLanguage(app)
        app.launchEnvironment["KINOWO_CLEAR_CITY"] = "1"
        app.launchEnvironment["KINOWO_FORCE_DETECTED_CITY"] = "warszawa"
        app.launch()

        let chooseOther = app.buttons[A11y.CityGate.chooseOtherButton]
        XCTAssertTrue(chooseOther.waitForExistence(timeout: 10),
                      "Confirm screen never offered the 'choose another city' button")
        chooseOther.tap()
    }

    override func tearDownWithError() throws { app = nil }

    func testTypingNarrowsTheCityListDiacriticInsensitively() throws {
        // Poland is grouped by voivodeship, "Dolnośląskie" leading the
        // Polish-collated rows, so it's on-screen before any query — proof the
        // manual list rendered. (Wrocław sits inside that group, hence not in
        // the accessibility tree until a query flattens it up.)
        XCTAssertTrue(app.buttons["Dolnośląskie"].waitForExistence(timeout: 5),
                      "Manual city list never appeared")

        // "wroc" — typed without diacritics — narrows to Wrocław alone: it gets
        // pulled into view, and the previously-visible voivodeship drops out.
        let search = app.textFields[A11y.CityGate.searchField]
        XCTAssertTrue(search.waitForExistence(timeout: 5), "No search field on the picker")
        search.tap()
        search.typeText("wroc")

        XCTAssertTrue(app.buttons["Wrocław"].waitForExistence(timeout: 3),
                      "Wrocław was filtered out by a query that should match it")
        XCTAssertFalse(app.buttons["Dolnośląskie"].exists,
                       "Dolnośląskie is still shown after searching 'wroc'")
    }

    func testDiacriticTypedQueryFindsThePolishCity() throws {
        // "lodz" must find "Łódź" (ł/ó folded away).
        let search = app.textFields[A11y.CityGate.searchField]
        XCTAssertTrue(search.waitForExistence(timeout: 5))
        search.tap()
        search.typeText("lodz")

        XCTAssertTrue(app.buttons["Łódź"].waitForExistence(timeout: 3),
                      "'lodz' did not surface 'Łódź'")
        // Dolnośląskie led the unfiltered list; "lodz" must have dropped it.
        XCTAssertFalse(app.buttons["Dolnośląskie"].exists)
    }
}

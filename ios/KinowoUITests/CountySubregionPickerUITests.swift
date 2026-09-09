import XCTest

/// The UK picker's THIRD step: a county that actually holds more than one
/// city (West Midlands → Birmingham/Dudley/Sandwell) gets its own tap and its
/// own back button, distinct from the second step's "back to regions". Every
/// other English county stays a flat row on the second step, unaffected.
final class CountySubregionPickerUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
        app.launchArguments += ["-UITests", "1"]
        app.launchEnvironment["KINOWO_CLEAR_CITY"] = "1"
        app.launchEnvironment["KINOWO_FORCE_DETECTED_CITY"] = "warszawa"
        // A prior interactive/test session's live `/api/catalog` fetch on this
        // simulator otherwise persists indefinitely and wins over the bundled
        // seed this test asserts on (see `CatalogStore`/`CatalogCache`).
        app.launchEnvironment["KINOWO_CLEAR_CATALOG_CACHE"] = "1"
        app.launch()

        let chooseOther = app.buttons[A11y.CityGate.chooseOtherButton]
        XCTAssertTrue(chooseOther.waitForExistence(timeout: 10),
                      "Confirm screen never offered the 'choose another city' button")
        chooseOther.tap()

        let uk = app.buttons["United Kingdom"]
        XCTAssertTrue(uk.waitForExistence(timeout: 5), "No 'United Kingdom' country pill on the picker")
        uk.tap()

        let england = app.buttons["England"]
        XCTAssertTrue(england.waitForExistence(timeout: 5), "No 'England' region row after switching to the UK")
        england.tap()
    }

    override func tearDownWithError() throws { app = nil }

    func testWestMidlandsOpensItsOwnCitiesAndTheOtherCountiesStayFlat() throws {
        // West Midlands is the one English county holding more than one city —
        // it shows as a group row, not a direct city, on England's list.
        let westMidlands = app.buttons["West Midlands"]
        XCTAssertTrue(westMidlands.waitForExistence(timeout: 5), "No 'West Midlands' group row under England")
        // A collapsed county (Cheshire) sits right there as a direct row —
        // proof the second step still mixes groups and direct cities.
        XCTAssertTrue(app.buttons["Cheshire"].exists, "Cheshire should be a direct row on England's list")
        // Its members are not listed individually yet.
        XCTAssertFalse(app.buttons["Birmingham"].exists, "Birmingham should be behind the West Midlands group")

        westMidlands.tap()

        XCTAssertTrue(app.buttons["Birmingham"].waitForExistence(timeout: 5))
        XCTAssertTrue(app.buttons["Dudley"].exists)
        XCTAssertTrue(app.buttons["Sandwell"].exists)
        // Nothing from the rest of England leaks into the county's own list.
        XCTAssertFalse(app.buttons["Cheshire"].exists, "Cheshire should not appear inside West Midlands")
    }

    func testBackFromASubregionReturnsToItsRegionNotTheTopRegionList() throws {
        app.buttons["West Midlands"].tap()
        XCTAssertTrue(app.buttons["Birmingham"].waitForExistence(timeout: 5))

        let back = app.buttons[A11y.CityGate.backToRegionButton]
        XCTAssertTrue(back.waitForExistence(timeout: 5), "No back button on the subregion step")
        back.tap()

        // Back on England's list — West Midlands is a group row again, and the
        // other counties (dropped out of view while browsing the subregion)
        // are back too, proving the query/step reset rather than landing on
        // the top nation list.
        XCTAssertTrue(app.buttons["West Midlands"].waitForExistence(timeout: 5),
                      "Did not return to England's own list")
        XCTAssertTrue(app.buttons["Cheshire"].exists)
        XCTAssertFalse(app.buttons["Birmingham"].exists, "Still showing West Midlands' cities after going back")
        // The nation list itself never reappeared — only one "back" was hit.
        XCTAssertFalse(app.buttons["Scotland"].exists)
    }
}

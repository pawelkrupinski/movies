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
        // The UK country pill now renders through the `country.<code>`
        // catalog key (`CountryDisplayName.localized`), so it follows the
        // resolved UI language rather than always reading its own native
        // name "United Kingdom" — forcing English here, same as
        // `FixtureLaunch.intoGrid`'s `-selectedLanguageCode`/`-AppleLanguages`/
        // `-AppleLocale` trio, is what makes that lookup below deterministic.
        // Without it this test inherited whatever language a PREVIOUS test
        // class's launch left persisted in `UserDefaults`/`AppleLanguages` —
        // harmless before this change (the old `Country.displayName` never
        // translated), but this test would otherwise silently depend on
        // suite run order once it did.
        app.launchArguments += [
            "-selectedLanguageCode", "en",
            "-AppleLanguages", "(en)",
            "-AppleLocale", "en",
        ]
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
        // West Midlands now sits interleaved in its own alphabetical position
        // among England's ~49 rows (the fix for the "West Midlands out of
        // order" bug), which is well below the fold — search narrows to it,
        // same as `CityChoiceSearchUITests` does for a city below the fold.
        // "cheshire" (a collapsed county reading as a direct row) sits well
        // ABOVE the fold and needs no search — proof this step still mixes
        // group headings and direct cities, same list.
        XCTAssertTrue(app.buttons["Cheshire"].waitForExistence(timeout: 5),
                      "Cheshire should be a direct row on England's list")

        let search = app.textFields[A11y.CityGate.searchField]
        XCTAssertTrue(search.waitForExistence(timeout: 5), "No search field on the picker")
        search.tap()
        search.typeText("west mid")

        // West Midlands is the one English county holding more than one city —
        // it shows as a group row, not a direct city, on England's list.
        let westMidlands = app.buttons["West Midlands"]
        XCTAssertTrue(westMidlands.waitForExistence(timeout: 5), "No 'West Midlands' group row under England")
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
        // West Midlands sits well below the fold in England's alphabetical
        // list — search narrows to it, same as the test above.
        let search = app.textFields[A11y.CityGate.searchField]
        XCTAssertTrue(search.waitForExistence(timeout: 5), "No search field on the picker")
        search.tap()
        search.typeText("west mid")
        app.buttons["West Midlands"].tap()
        XCTAssertTrue(app.buttons["Birmingham"].waitForExistence(timeout: 5))

        let back = app.buttons[A11y.CityGate.backToRegionButton]
        XCTAssertTrue(back.waitForExistence(timeout: 5), "No back button on the subregion step")
        back.tap()

        // Back on England's list, query reset — "Cheshire" (well above the
        // fold) is back, proving the step reset rather than landing on the
        // top nation list. Re-search to confirm West Midlands is a group row
        // again too, not still showing its own cities.
        XCTAssertTrue(app.buttons["Cheshire"].waitForExistence(timeout: 5),
                      "Did not return to England's own list")
        XCTAssertFalse(app.buttons["Birmingham"].exists, "Still showing West Midlands' cities after going back")
        // The nation list itself never reappeared — only one "back" was hit.
        XCTAssertFalse(app.buttons["Scotland"].exists)

        search.tap()
        search.typeText("west mid")
        XCTAssertTrue(app.buttons["West Midlands"].waitForExistence(timeout: 5),
                      "West Midlands should be a group row again after going back")
    }
}

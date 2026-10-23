import XCTest

/// The UK picker's THIRD step: a county that actually holds more than one
/// city in the bundled seed (Glamorgan → Cardiff, Swansea…) gets its own tap
/// and its own back button, distinct from the second step's "back to
/// regions". A place no county splits off (Builth Wells) stays a direct row
/// on the same second-step list. This runs against the bundled seed, so the
/// names it taps must still be what the seed says.
@MainActor
final class CountySubregionPickerUITests: XCTestCase {
    var app: XCUIApplication!

    /// The one Welsh place the seed files under no county.
    private static let directCity = "Builth Wells area"

    override func setUp() async throws {
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

        let wales = app.buttons["Wales"]
        XCTAssertTrue(wales.waitForExistence(timeout: 5), "No 'Wales' region row after switching to the UK")
        wales.tap()
    }

    override func tearDown() async throws { app = nil }

    func testGlamorganOpensItsOwnCitiesAndTheOtherCountiesStayFlat() throws {
        // "Builth Wells area" (in no county group, reading as a direct row)
        // sits on the same list — proof this step still mixes group headings
        // and direct cities.
        XCTAssertTrue(app.buttons[Self.directCity].waitForExistence(timeout: 5),
                      "\(Self.directCity) should be a direct row on Wales' list")

        // Glamorgan holds more than one city — it shows as a group row, and its members are not listed individually yet.
        let glamorgan = app.buttons["Glamorgan"]
        XCTAssertTrue(glamorgan.waitForExistence(timeout: 5), "No 'Glamorgan' group row under Wales")
        XCTAssertFalse(app.buttons["Cardiff"].exists, "Cardiff should be behind the Glamorgan group")

        glamorgan.tap()

        XCTAssertTrue(app.buttons["Cardiff"].waitForExistence(timeout: 5))
        XCTAssertTrue(app.buttons["Swansea"].exists, "Swansea should be listed inside Glamorgan")
        // Nothing from the rest of Wales leaks into the county's own list.
        XCTAssertFalse(app.buttons["Gwent"].exists, "Gwent should not appear inside Glamorgan")
        XCTAssertFalse(app.buttons[Self.directCity].exists, "\(Self.directCity) should not appear inside Glamorgan")
    }

    func testBackFromASubregionReturnsToItsRegionNotTheTopRegionList() throws {
        let glamorgan = app.buttons["Glamorgan"]
        XCTAssertTrue(glamorgan.waitForExistence(timeout: 5), "No 'Glamorgan' group row under Wales")
        glamorgan.tap()
        XCTAssertTrue(app.buttons["Cardiff"].waitForExistence(timeout: 5))

        let back = app.buttons[A11y.CityGate.backToRegionButton]
        XCTAssertTrue(back.waitForExistence(timeout: 5), "No back button on the subregion step")
        back.tap()

        // Back on Wales' own list — its direct city is back and Cardiff is
        // behind its group again, proving the step reset rather than landing
        // on the top nation list.
        XCTAssertTrue(app.buttons[Self.directCity].waitForExistence(timeout: 5),
                      "Did not return to Wales' own list")
        XCTAssertFalse(app.buttons["Cardiff"].exists, "Still showing Glamorgan's cities after going back")
        XCTAssertTrue(app.buttons["Glamorgan"].exists, "Glamorgan should be a group row again after going back")
        // The nation list itself never reappeared — only one "back" was hit.
        XCTAssertFalse(app.buttons["England"].exists)
    }
}

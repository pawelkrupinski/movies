import XCTest

/// A deterministic launch into the film grid, shared by the suites that need to
/// be ON the listing before they can assert anything.
///
/// Launching with just `-UITests 1` is not enough: nothing pins a city, so on a
/// simulator with no persisted choice the app stops at the CityGate and the grid
/// never appears. That made those suites quietly dependent on some earlier test
/// having picked a city, and fail outright whenever they ran first or on a fresh
/// simulator. Forcing the city — and serving the offline fixture repertoire, so
/// nothing rides on live network timing or on the listing being non-empty late
/// at night — is what makes them reproducible.
enum FixtureLaunch {

    /// Launch `app` on `city`'s fixture listing and return once the grid is up.
    ///
    /// `country` / `language` land in `UserDefaults`' ARGUMENT domain, which
    /// outranks anything persisted on the simulator — the app forces the
    /// selected country's language, and iOS fixes the bundle's localization at
    /// process start, so both have to be set here to get one consistent
    /// language on the first launch rather than a relaunch dance.
    static func intoGrid(
        _ app: XCUIApplication,
        country: String = "pl",
        language: String = "pl",
        city: String = "warszawa",
        environment: [String: String] = [:],
        file: StaticString = #filePath,
        line: UInt = #line
    ) {
        app.launchArguments += [
            "-UITests", "1",
            "-selectedCountryCode", country,
            "-AppleLanguages", "(\(language))",
            "-AppleLocale", language,
        ]
        app.launchEnvironment["KINOWO_UITEST_FIXTURE"] = "1"
        app.launchEnvironment["KINOWO_CLEAR_CITY"] = "1"
        app.launchEnvironment["KINOWO_FORCE_DETECTED_CITY"] = city
        for (key, value) in environment { app.launchEnvironment[key] = value }
        app.launch()

        let confirm = app.buttons[A11y.CityGate.confirmButton]
        XCTAssertTrue(confirm.waitForExistence(timeout: 15),
                      "City-confirm screen never showed", file: file, line: line)
        confirm.tap()
    }

    /// The first film card — the marker that the grid has actually rendered.
    static func firstFilmCard(_ app: XCUIApplication) -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: A11y.FilmGrid.cell)
            .firstMatch
    }
}

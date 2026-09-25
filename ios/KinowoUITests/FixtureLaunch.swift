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
    /// `country` and `language` are independent (see `LanguageSelection`) and
    /// land in `UserDefaults`' ARGUMENT domain, which outranks anything
    /// persisted on the simulator:
    ///
    /// - `-selectedCountryCode` is `CountrySelection.key` — read directly by
    ///   `CountrySelection.current`.
    /// - `-selectedLanguageCode` is `LanguageSelection.key` — read directly by
    ///   `LanguageSelection.explicit`, so it wins step 1 of the resolution
    ///   algorithm regardless of the device's own preferred language.
    /// - `-AppleLanguages` / `-AppleLocale` additionally fix the bundle's
    ///   localization (what `String(localized:)` resolves against) and
    ///   `Locale.preferredLanguages` at process start, so one consistent
    ///   language lands on the very first launch with no relaunch dance.
    static func intoGrid(
        _ app: XCUIApplication,
        country: String = "pl",
        language: String = "pl",
        city: String = "warszawa",
        environment: [String: String] = [:],
        file: StaticString = #filePath,
        line: UInt = #line
    ) {
        app.launchEnvironment["KINOWO_UITEST_FIXTURE"] = "1"
        throughCityGate(app, country: country, language: language, city: city,
                        environment: environment, file: file, line: line)
    }

    /// Launch `app` with no persisted city and confirm `city` at the CityGate,
    /// against whatever repertoire the launch environment selects (the live one
    /// unless `KINOWO_UITEST_FIXTURE` is set).
    ///
    /// The gate ALWAYS shows, whatever an earlier suite persisted, so it is
    /// awaited as a requirement. An `if confirm.waitForExistence(timeout: 10)`
    /// instead made the launch depend on run order, and cost ten idle seconds
    /// per test whenever a city was already stored (run 36115063750).
    static func throughCityGate(
        _ app: XCUIApplication,
        country: String = "pl",
        language: String = "pl",
        city: String,
        environment: [String: String] = [:],
        file: StaticString = #filePath,
        line: UInt = #line
    ) {
        pinCountryAndLanguage(app, country: country, language: language)
        app.launchEnvironment["KINOWO_CLEAR_CITY"] = "1"
        app.launchEnvironment["KINOWO_FORCE_DETECTED_CITY"] = city
        for (key, value) in environment { app.launchEnvironment[key] = value }
        app.launch()

        let confirm = app.buttons[A11y.CityGate.confirmButton]
        XCTAssertTrue(confirm.waitForExistence(timeout: 15),
                      "City-confirm screen never showed", file: file, line: line)
        confirm.tap()
        // Name a tap the gate swallowed here, rather than as a later "Grid never appeared".
        XCTAssertTrue(confirm.waitForNonExistence(timeout: 10),
                      "Confirming the city left the CityGate up", file: file, line: line)
    }

    /// Mark `app` as a UI-test launch and pin its country and UI language in
    /// `UserDefaults`' ARGUMENT domain (see `intoGrid` for what each key does).
    ///
    /// Every suite that launches the app must go through this (or `intoGrid`),
    /// never a bare `-UITests 1`: the simulator keeps whatever an EARLIER suite
    /// persisted — `CountySubregionPickerUITests` switches to the UK under
    /// English — so an unpinned launch would fetch `showtimes.cc/uk/warszawa`
    /// (a 404, "Couldn't load showtimes") and render English day labels,
    /// failing on run order alone. Only a suite that changes country through
    /// the UI itself may skip it, since the argument domain would override
    /// that change.
    static func pinCountryAndLanguage(
        _ app: XCUIApplication,
        country: String = "pl",
        language: String = "pl"
    ) {
        app.launchArguments += [
            "-UITests", "1",
            "-selectedCountryCode", country,
            "-selectedLanguageCode", language,
            "-AppleLanguages", "(\(language))",
            "-AppleLocale", language,
        ]
    }

    /// The first film card — the marker that the grid has actually rendered.
    static func firstFilmCard(_ app: XCUIApplication) -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: A11y.FilmGrid.cell)
            .firstMatch
    }
}

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

    /// Launch `app` on `city`'s fixture listing (the caller waits for the grid).
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
        environment: [String: String] = [:]
    ) {
        app.launchEnvironment["KINOWO_UITEST_FIXTURE"] = "1"
        intoCity(app, country: country, language: language, city: city, environment: environment)
    }

    /// Launch `app` straight onto `city`'s repertoire — the live one unless
    /// `KINOWO_UITEST_FIXTURE` is set — without passing through the CityGate.
    ///
    /// The city is pinned like the country (`-selectedCity`, read once by
    /// `UserPreferences.init`), so whatever an earlier suite persisted cannot
    /// leak in, and no tap is spent getting past the gate. Confirming it with a
    /// tap was the one synthesized gesture every such test paid, and a
    /// synthesized touch is occasionally lost below the app: in local run
    /// 2026-09-25 backboardd registered XCTest's virtual digitizer twice
    /// ("unknown digitizer"), dropped the touch's move/up ("didn't see a previous
    /// touch down"), and the gate stayed up, failing
    /// `testGermanyRendersGermanDetailCaptions` with "Grid never appeared" (1 of
    /// ~150 gestures in that run). The gate's own suites still tap it.
    ///
    /// `KINOWO_FORCE_DETECTED_CITY` stays set so a test that re-arms the gate
    /// in-session (a country switch, "pick another city") lands on the confirm
    /// screen instead of a CoreLocation permission dialog.
    static func intoCity(
        _ app: XCUIApplication,
        country: String = "pl",
        language: String = "pl",
        city: String,
        environment: [String: String] = [:]
    ) {
        pinCountryAndLanguage(app, country: country, language: language)
        app.launchArguments += ["-selectedCity", city]
        app.launchEnvironment["KINOWO_FORCE_DETECTED_CITY"] = city
        for (key, value) in environment { app.launchEnvironment[key] = value }
        app.launch()
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

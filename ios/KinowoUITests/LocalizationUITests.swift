import XCTest

/// The app ships a `pl` / `en` / `de` string catalog, and everything the user
/// reads is supposed to come out of it. For a long time most of the SwiftUI
/// chrome was hardcoded Polish literals instead, so a German or British user
/// got a Polish filter sheet — the catalog only covered the city gate and a
/// handful of other screens.
///
/// These launch the app under a forced language and read back what actually
/// rendered. They fail against the hardcoded literals (every locale returned
/// the Polish text) and pass once the views resolve their captions through the
/// catalog. Two elements are enough to prove the wiring, and both are on the
/// top bar so no navigation or tapping is needed:
///
/// - the rightmost date pill, whose caption comes from `DateFilter.label` —
///   the `String(localized:)` path that had to move out of `KinowoCore`;
/// - the Filtry button's accessibility label, a plain `LocalizedStringKey`.
final class LocalizationUITests: XCTestCase {

    private var app: XCUIApplication!

    /// Put the simulator back on Polish before handing over to the next test.
    ///
    /// `KinowoApp.init` persists `AppleLanguages` from the selected country on
    /// every launch, so a run that forced `uk` leaves `["en"]` written to the
    /// app's defaults. The country code itself doesn't persist (we only inject
    /// it into the argument domain), so the *next* launch re-derives Polish and
    /// self-heals — but iOS fixes the bundle's localization at process start,
    /// so that one launch still comes up in the previous language. Left alone,
    /// this hands whichever test runs first in the next suite a mis-localized
    /// app. One throwaway Polish launch here closes that window.
    override func tearDownWithError() throws {
        app = nil
        let reset = XCUIApplication()
        reset.launchArguments += [
            "-UITests", "1",
            "-selectedCountryCode", "pl",
            "-AppleLanguages", "(pl)",
            "-AppleLocale", "pl",
        ]
        reset.launch()
        reset.terminate()
    }

    func testPolandRendersPolishCaptions() throws {
        launch(country: "pl", language: "pl")
        assertTopBarReads(datePills: ["Dziś", "Jutro", "7 dni", "Wszystkie"],
                          searchPlaceholder: "Szukaj filmu",
                          filtersButton: "Filtry")
    }

    func testUnitedKingdomRendersEnglishCaptions() throws {
        launch(country: "uk", language: "en")
        assertTopBarReads(datePills: ["Today", "Tomorrow", "7 days", "All"],
                          searchPlaceholder: "Search for a film",
                          filtersButton: "Filters")
    }

    func testGermanyRendersGermanCaptions() throws {
        launch(country: "de", language: "de")
        assertTopBarReads(datePills: ["Heute", "Morgen", "7 Tage", "Alle"],
                          searchPlaceholder: "Film suchen",
                          filtersButton: "Filter")
    }

    // MARK: - helpers

    /// The app doesn't follow the device language: it forces the selected
    /// country's one (`CountrySelection`). That reaches the UI by two separate
    /// routes, and a caption is localized only if BOTH are pointed at the same
    /// place — which is exactly what these tests are here to pin down:
    ///
    /// - `selectedCountryCode` drives the root `.environment(\.locale)`, which
    ///   is what SwiftUI resolves a `LocalizedStringKey` against;
    /// - `AppleLanguages` picks the bundle `String(localized:)` reads.
    ///
    /// `KinowoApp.init` normally derives the second from the first, but only
    /// for the *next* launch — iOS fixes the bundle's localization at process
    /// start. Setting both here (they land in `UserDefaults`' argument domain,
    /// which outranks anything persisted on the simulator) gets one consistent
    /// language on the first launch, with no relaunch dance.
    private func launch(country: String, language: String) {
        app = XCUIApplication()
        app.launchArguments += [
            "-UITests", "1",
            "-selectedCountryCode", country,
            "-AppleLanguages", "(\(language))",
            "-AppleLocale", language,
        ]
        app.launch()
    }

    private func assertTopBarReads(
        datePills: [String],
        searchPlaceholder: String,
        filtersButton: String,
        file: StaticString = #filePath,
        line: UInt = #line
    ) {
        let ids = [
            A11y.TopBar.datePillToday,
            A11y.TopBar.datePillTomorrow,
            A11y.TopBar.datePillWeek,
            A11y.TopBar.datePillAnytime,
        ]
        XCTAssertTrue(app.buttons[ids[0]].waitForExistence(timeout: 20),
                      "Top bar never appeared", file: file, line: line)
        // `DateFilter.label` — resolved through `String(localized:)`, because
        // the pill row measures the rendered width of each caption.
        XCTAssertEqual(ids.map { app.buttons[$0].label }, datePills,
                       "Date pill captions", file: file, line: line)

        // A plain `LocalizedStringKey` handed to `TextField`, i.e. the other
        // half of how captions reach the screen.
        let search = app.textFields[A11y.Search.field]
        XCTAssertTrue(search.waitForExistence(timeout: 5),
                      "Search field missing", file: file, line: line)
        XCTAssertEqual(search.placeholderValue, searchPlaceholder,
                       "Search field placeholder", file: file, line: line)

        // Icon-only button: proves our `.accessibilityLabel` overrides the SF
        // Symbol's own system name, which reads "Filter" in every language.
        let filters = app.buttons[A11y.TopBar.filtryButton]
        XCTAssertTrue(filters.waitForExistence(timeout: 5),
                      "Filtry button missing", file: file, line: line)
        XCTAssertEqual(filters.label, filtersButton,
                       "Filtry button accessibility label", file: file, line: line)
    }
}

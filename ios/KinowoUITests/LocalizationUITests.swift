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
        // Launched, deliberately NOT terminated. `KinowoApp.init` persists the
        // Polish defaults during startup, so the reset has already landed by the
        // time `launch()` returns — terminating afterwards bought nothing and
        // cost the whole suite its result: XCTest saw the app die outside a
        // test's own lifecycle, logged "Restarting after unexpected exit", and
        // marked the run FAILED with an empty "Failing tests:" list even though
        // all six passed. Leaving it running is harmless; the next test's
        // `launch()` replaces it.
        reset.launch()
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

    func testSpainRendersSpanishCaptions() throws {
        launch(country: "es", language: "es")
        assertTopBarReads(datePills: ["Hoy", "Mañana", "7 días", "Todo"],
                          searchPlaceholder: "Busca una película",
                          filtersButton: "Filtros")
    }

    // MARK: - detail screen
    //
    // Its meta-block captions are passed to `metaBlock` as a `String` (it
    // uppercases them), so Xcode's extractor never saw them and they stayed
    // Polish in every locale long after the rest of the UI was translated.
    // These cover the same trap on the screen where it actually bit.

    func testPolandRendersPolishDetailCaptions() throws {
        launchDetail(country: "pl", language: "pl")
        assertDetailReads(["REŻYSERIA", "OBSADA", "KRAJ(E) PRODUKCJI"])
    }

    func testUnitedKingdomRendersEnglishDetailCaptions() throws {
        launchDetail(country: "uk", language: "en")
        assertDetailReads(["DIRECTOR", "CAST", "COUNTRIES"])
    }

    func testGermanyRendersGermanDetailCaptions() throws {
        launchDetail(country: "de", language: "de")
        assertDetailReads(["REGIE", "BESETZUNG", "LÄNDER"])
    }

    func testSpainRendersSpanishDetailCaptions() throws {
        launchDetail(country: "es", language: "es")
        assertDetailReads(["DIRECTOR", "REPARTO", "PAÍSES"])
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
    /// start. `FixtureLaunch` sets both, so one consistent language lands on
    /// the first launch with no relaunch dance; it also forces the city and
    /// serves the offline fixture, which is what makes these reproducible on a
    /// fresh simulator. See `FixtureLaunch` for the full reasoning.
    private func launch(country: String, language: String,
                        file: StaticString = #filePath, line: UInt = #line) {
        app = XCUIApplication()
        FixtureLaunch.intoGrid(app, country: country, language: language,
                               file: file, line: line)
    }

    /// As `launch`, then opens the first film's detail screen.
    private func launchDetail(country: String, language: String,
                              file: StaticString = #filePath, line: UInt = #line) {
        launch(country: country, language: language, file: file, line: line)

        let card = FixtureLaunch.firstFilmCard(app)
        XCTAssertTrue(card.waitForExistence(timeout: 30),
                      "Grid never appeared", file: file, line: line)
        // Tap the poster region: the rating links and showtime chips hold their
        // own hit areas, so a centre tap can miss the NavigationLink.
        card.coordinate(withNormalizedOffset: CGVector(dx: 0.5, dy: 0.18)).tap()
    }

    /// Meta-block captions render uppercased, so `expected` is uppercase too.
    private func assertDetailReads(_ expected: [String],
                                   file: StaticString = #filePath, line: UInt = #line) {
        for caption in expected {
            XCTAssertTrue(app.staticTexts[caption].waitForExistence(timeout: 10),
                          "Detail screen is missing the \(caption) meta block",
                          file: file, line: line)
        }
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

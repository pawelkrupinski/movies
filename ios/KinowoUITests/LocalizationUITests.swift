import XCTest

/// The app ships a `pl` / `en` / `de` / `es` string catalog, and everything
/// the user reads is supposed to come out of it. For a long time most of the
/// SwiftUI chrome was hardcoded Polish literals instead, so a German or
/// British user got a Polish filter sheet — the catalog only covered the city
/// gate and a handful of other screens.
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
///
/// The UI language is a fully independent preference from country (see
/// `LanguageSelection`) — the two used to be hard-coupled (`CountrySelection`
/// forced a country's own language), so every existing case below still pairs
/// them 1:1 for simplicity, but `testSwitchingCountryDoesNotChangeLanguage`
/// below is the one that actually pins the decoupling: a country/language
/// combination (UK + German) that was impossible to express before this
/// change, and a language that survives an in-app country switch.
final class LocalizationUITests: XCTestCase {

    private var app: XCUIApplication!

    /// Put the simulator back on Polish before handing over to the next test.
    ///
    /// `KinowoApp.init` persists `AppleLanguages` from the resolved language
    /// (`LanguageSelection.resolve`) on every launch, so a run that forced
    /// `de` leaves `["de"]` written to the app's defaults. Neither the country
    /// code nor the language code persists across launches on its own (we only
    /// inject them into the argument domain), so the *next* launch re-derives
    /// Polish and self-heals — but iOS fixes the bundle's localization at
    /// process start, so that one launch still comes up in the previous
    /// language. Left alone, this hands whichever test runs first in the next
    /// suite a mis-localized app. One throwaway Polish launch here closes that
    /// window.
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

    // MARK: - decoupling (country ≠ language)

    /// The regression this change exists for: UK + German is a combination
    /// `CountrySelection` couldn't even express before (the UK deployment's
    /// own `languageCode` is `en`, so the old coupled code always forced
    /// English there). Launches into it, confirms German actually rendered,
    /// then drives the in-app country switch (Filtry → "choose another city"
    /// → the manual picker's country pills, exactly as `PickAnotherCityUITests`
    /// does) to Poland and asserts the top bar is STILL German — not Polish,
    /// which is what the old `CountrySelection.select`-forces-`AppleLanguages`
    /// coupling would have produced.
    func testSwitchingCountryDoesNotChangeLanguage() throws {
        launch(country: "uk", language: "de")
        assertTopBarReads(datePills: ["Heute", "Morgen", "7 Tage", "Alle"],
                          searchPlaceholder: "Film suchen",
                          filtersButton: "Filter")

        app.buttons[A11y.TopBar.filtryButton].tap()

        // The button sits near the bottom of the Filtry `Form` — scroll it
        // into view, same as `PickAnotherCityUITests`.
        let pickButton = app.buttons[A11y.FiltersSheet.pickAnotherCityButton]
        for _ in 0..<8 where !pickButton.exists {
            app.swipeUp()
        }
        XCTAssertTrue(pickButton.exists, "Filtry never showed the 'Choose another city' button")
        pickButton.tap()

        let confirm = app.buttons[A11y.CityGate.confirmButton]
        XCTAssertTrue(confirm.waitForExistence(timeout: 10), "CityGate never re-armed")
        app.buttons[A11y.CityGate.chooseOtherButton].tap()

        // Country pills now resolve through the `country.<code>` catalog key
        // (see `CityGate`'s `countryPicker`), so the pill reads Poland's name
        // in the CURRENTLY FORCED UI language — still German here, since only
        // the country is switching — rather than "Polska", Poland's own name.
        let poland = app.buttons["Polen"]
        XCTAssertTrue(poland.waitForExistence(timeout: 10), "Country picker never showed Poland")
        poland.tap()

        // Poland is a flat (unregioned) country, so its city list renders
        // directly — but "Warszawa" sits below the fold of the alphabetical
        // list, so it isn't in the accessibility tree until the search field
        // narrows to it (same reasoning as `CityChoiceSearchUITests`).
        let search = app.searchFields.firstMatch
        XCTAssertTrue(search.waitForExistence(timeout: 10), "No search field on the picker")
        search.tap()
        search.typeText("warszawa")

        let warszawa = app.buttons["Warszawa"]
        XCTAssertTrue(warszawa.waitForExistence(timeout: 10), "Poland's city list never appeared")
        warszawa.tap()

        // Back on the grid, now under Poland — and still German, proving the
        // country switch left the independent language preference untouched.
        assertTopBarReads(datePills: ["Heute", "Morgen", "7 Tage", "Alle"],
                          searchPlaceholder: "Film suchen",
                          filtersButton: "Filter")
    }

    /// The regression this follow-up exists for: a country's OWN name must
    /// follow the resolved UI language on the picker (`Country.displayName`'s
    /// replacement, the `country.<code>` catalog lookup in `CityGate`'s
    /// `countryPicker`), while a CITY's name is the city's own name and must
    /// never translate — "Warszawa" doesn't become "Warschau" just because
    /// the reader's language is German. Same navigation as
    /// `testSwitchingCountryDoesNotChangeLanguage` (Filtry → "choose another
    /// city" → the manual picker), but this one stays on the country step
    /// long enough to assert BOTH pills read their German translation before
    /// moving on to Poland's (untranslated) city list. Fails before the
    /// `country.<code>` lookup landed — the pills read "Polska"/"United
    /// Kingdom" regardless of language — and passes after.
    func testCountryPickerTranslatesCountryNamesButNotCityNames() throws {
        launch(country: "uk", language: "de")

        app.buttons[A11y.TopBar.filtryButton].tap()

        // Same scroll-into-view as `testSwitchingCountryDoesNotChangeLanguage`.
        let pickButton = app.buttons[A11y.FiltersSheet.pickAnotherCityButton]
        for _ in 0..<8 where !pickButton.exists {
            app.swipeUp()
        }
        XCTAssertTrue(pickButton.exists, "Filtry never showed the 'Choose another city' button")
        pickButton.tap()

        let confirm = app.buttons[A11y.CityGate.confirmButton]
        XCTAssertTrue(confirm.waitForExistence(timeout: 10), "CityGate never re-armed")
        app.buttons[A11y.CityGate.chooseOtherButton].tap()

        // Both pills read their GERMAN translation, not their own native
        // name — Poland's own name is "Polska", the UK's is "United Kingdom";
        // neither string should appear on this screen any more.
        let poland = app.buttons["Polen"]
        XCTAssertTrue(poland.waitForExistence(timeout: 10),
                      "Country picker never showed Poland's German name")
        XCTAssertTrue(app.buttons["Vereinigtes Königreich"].exists,
                      "Country picker never showed the UK's German name")
        XCTAssertFalse(app.buttons["Polska"].exists,
                       "Poland pill still read its own native name, not the German translation")
        XCTAssertFalse(app.buttons["United Kingdom"].exists,
                       "UK pill still read its own native name, not the German translation")

        poland.tap()

        // Poland's city list — untranslated: "Warszawa" sits below the fold
        // of the alphabetical list, so search narrows to it, same reasoning
        // as `testSwitchingCountryDoesNotChangeLanguage`. It must still read
        // "Warszawa", not a German rendering, because a city's name is the
        // city's own name and never runs through the language catalog.
        let search = app.searchFields.firstMatch
        XCTAssertTrue(search.waitForExistence(timeout: 10), "No search field on the picker")
        search.tap()
        search.typeText("warszawa")

        let warszawa = app.buttons["Warszawa"]
        XCTAssertTrue(warszawa.waitForExistence(timeout: 10),
                      "Poland's city list never showed Warszawa under its own, untranslated name")
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

    /// The UI language is independent of country (`LanguageSelection`) and
    /// reaches the UI by two separate routes, both of which a caption must
    /// come through to render localized:
    ///
    /// - `selectedLanguageCode` (`LanguageSelection.key`) drives the root
    ///   `.environment(\.locale)`, which is what SwiftUI resolves a
    ///   `LocalizedStringKey` against;
    /// - `AppleLanguages` picks the bundle `String(localized:)` reads.
    ///
    /// `KinowoApp.init` normally derives the second from the first (via
    /// `LanguageSelection.resolve`), but only for the *next* launch — iOS
    /// fixes the bundle's localization at process start. `FixtureLaunch` sets
    /// both directly, so one consistent language lands on the first launch
    /// with no relaunch dance; it also forces the city and serves the offline
    /// fixture, which is what makes these reproducible on a fresh simulator.
    /// See `FixtureLaunch` for the full reasoning.
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

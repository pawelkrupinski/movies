import XCTest

/// Gate for the card-spacing tuning levers: dragging a spacing slider on the
/// non-prod `ShowtimeTuningScreen` must actually move the gaps in the real
/// `FilmCardView` it renders.
///
/// We drive the "Sekcje" slider (`CardSpacingStyle.sectionSpacing`, the inner
/// `VStack(spacing:)` shared by title→meta→ratings→showings) and measure the
/// vertical gap between the first card's title and its first day label —
/// three `sectionSpacing` gaps sit between them, so pushing it from the default
/// 8pt to the 24pt maximum widens that gap by a clearly measurable amount. Both
/// anchors sit near the top of the card, above the controls sheet, so neither
/// is clipped (measuring the whole card's height fails — the card is taller
/// than the visible strip above the sheet, so its frame is clamped). The
/// measurement is scoped to the first card so the other cards' identical day
/// labels don't interfere.
///
/// Before this feature existed there was no such slider and no
/// `cardSpacingStyle` wiring, so the gap couldn't move — it fails before,
/// passes after. Reached only via the `KINOWO_TUNING` launch env (DEBUG).
///
/// The Kina / Film tests below ride the same harness: they switch the tuning
/// pager to the Kina or Film page and confirm that page's font slider moves a
/// real-view element (a cinema section header / the detail title). Those
/// sliders + the `CinemaHeaderStyle` / `FilmDetailStyle` wiring didn't exist
/// before — fail before, pass after.
final class CardSpacingTuningUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
        app.launchArguments += ["-UITests", "1"]
        app.launchEnvironment["KINOWO_TUNING"] = "1"
        app.launch()
    }

    override func tearDownWithError() throws { app = nil }

    func testSectionSpacingSliderWidensTitleToMetaGap() throws {
        let card = firstCard()
        XCTAssertTrue(card.waitForExistence(timeout: 20), "Tuning screen never showed its first card")

        // Both strings are unique to the first sample film ("1 — Wszystkie
        // przypadki", 128 min → "2h 8min"), so a global query lands on card 0.
        // They sit on consecutive rows separated by exactly one `sectionSpacing`
        // gap, near the top of the card and above the controls sheet — so
        // neither is clipped.
        let title = app.staticTexts["1 — Wszystkie przypadki"]
        let runtime = app.staticTexts["2h 8min"]
        XCTAssertTrue(title.waitForExistence(timeout: 5), "First card's title not found")
        XCTAssertTrue(runtime.waitForExistence(timeout: 5), "First card's runtime pill not found")

        let gapBefore = runtime.frame.minY - title.frame.maxY

        let slider = app.sliders[A11y.Tuning.sectionSpacingSlider]
        XCTAssertTrue(slider.waitForExistence(timeout: 5), "Section-spacing slider missing")
        slider.adjust(toNormalizedSliderPosition: 1.0) // 8 → 24pt
        Thread.sleep(forTimeInterval: 0.4) // let the layout settle

        let gapAfter = runtime.frame.minY - title.frame.maxY
        XCTAssertGreaterThan(
            gapAfter, gapBefore + 8,
            "Widening the section spacing to max barely moved the runtime row "
            + "(gap \(gapBefore) → \(gapAfter)pt) — the sectionSpacing lever isn't wired to FilmCardView."
        )
    }

    func testDayToCinemaSliderWidensDayHeaderToCinemaGap() throws {
        let card = firstCard()
        XCTAssertTrue(card.waitForExistence(timeout: 20), "Tuning screen never showed its first card")

        // Card 0 ("1 — Wszystkie przypadki"): the day label is uppercased by
        // ShowingsView; "Kino Pod Baranami" is its first cinema. firstMatch lands
        // on card 0 (topmost in the grid). The gap between them is `dayToCinema`.
        let dayLabel = app.staticTexts["PONIEDZIAŁEK 8 CZERWCA"].firstMatch
        let cinema = app.staticTexts["Kino Pod Baranami"].firstMatch
        XCTAssertTrue(dayLabel.waitForExistence(timeout: 5), "Day label not found on card 0")
        XCTAssertTrue(cinema.waitForExistence(timeout: 5), "Cinema label not found on card 0")
        let gapBefore = cinema.frame.minY - dayLabel.frame.maxY

        let slider = app.sliders[A11y.Tuning.dayToCinemaSlider]
        XCTAssertTrue(slider.waitForExistence(timeout: 5), "dayToCinema slider missing")
        if !slider.isHittable { app.scrollViews[A11y.Tuning.controlsScroll].swipeUp() }
        slider.adjust(toNormalizedSliderPosition: 1.0) // 6 → 24pt
        Thread.sleep(forTimeInterval: 0.4)

        let gapAfter = cinema.frame.minY - dayLabel.frame.maxY
        XCTAssertGreaterThan(
            gapAfter, gapBefore + 8,
            "Maxing dayToCinema barely moved the cinema name below the day label "
            + "(gap \(gapBefore) → \(gapAfter)pt) — the dayToCinema lever isn't wired to ShowingsView."
        )
    }

    func testKinaPageFontSliderGrowsCinemaHeader() throws {
        let card = firstCard()
        XCTAssertTrue(card.waitForExistence(timeout: 20), "Tuning screen never showed its first card")

        switchToPage(1) // Kina

        let header = app.staticTexts.matching(identifier: A11y.CinemaPage.sectionHeader).firstMatch
        XCTAssertTrue(header.waitForExistence(timeout: 5), "Kina page never showed a cinema section header")
        let heightBefore = header.frame.height

        let slider = app.sliders[A11y.Tuning.cinemaHeaderFontSlider]
        XCTAssertTrue(slider.waitForExistence(timeout: 5), "Cinema-header font slider missing on the Kina page")
        slider.adjust(toNormalizedSliderPosition: 1.0) // 15 → 24pt
        Thread.sleep(forTimeInterval: 0.4)

        let heightAfter = header.frame.height
        XCTAssertGreaterThan(
            heightAfter, heightBefore + 2,
            "Maxing the cinema-header font barely moved the header height "
            + "(\(heightBefore) → \(heightAfter)pt) — the CinemaHeaderStyle font lever isn't wired to CinemaSectionedGridView."
        )
    }

    func testFilmPageFontSliderGrowsDetailTitle() throws {
        let card = firstCard()
        XCTAssertTrue(card.waitForExistence(timeout: 20), "Tuning screen never showed its first card")

        switchToPage(2) // Film

        let title = app.staticTexts[A11y.Tuning.detailTitle]
        XCTAssertTrue(title.waitForExistence(timeout: 5), "Film page never showed the detail title")
        let heightBefore = title.frame.height

        let slider = app.sliders[A11y.Tuning.detailTitleFontSlider]
        XCTAssertTrue(slider.waitForExistence(timeout: 5), "Detail-title font slider missing on the Film page")
        if !slider.isHittable { app.scrollViews[A11y.Tuning.controlsScroll].swipeUp() }
        slider.adjust(toNormalizedSliderPosition: 1.0) // 22 → 34pt
        Thread.sleep(forTimeInterval: 0.4)

        let heightAfter = title.frame.height
        XCTAssertGreaterThan(
            heightAfter, heightBefore + 4,
            "Maxing the detail-title font barely moved the title height "
            + "(\(heightBefore) → \(heightAfter)pt) — the FilmDetailStyle title lever isn't wired to FilmDetailView."
        )
    }

    /// Jump the pager to a page by tapping its label in the page bar (more
    /// robust than a swipe, which depends on a distance threshold). `index`
    /// is the `TuningPage` raw value (0 = card, 1 = kina, 2 = film) — kept as
    /// a plain Int so the UI-test target needn't see the app's view types.
    private func switchToPage(_ index: Int) {
        let tab = app.buttons["\(A11y.Tuning.pageTabPrefix).\(index)"]
        XCTAssertTrue(tab.waitForExistence(timeout: 5), "Page tab \(index) missing")
        tab.tap()
        Thread.sleep(forTimeInterval: 0.4) // let the pager settle on the new page
    }

    private func firstCard() -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: "\(A11y.Tuning.cardPrefix).0")
            .firstMatch
    }
}

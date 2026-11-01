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
@MainActor
final class CardSpacingTuningUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUp() async throws {
        continueAfterFailure = false
        app = XCUIApplication()
        FixtureLaunch.pinCountryAndLanguage(app)
        app.launchEnvironment["KINOWO_TUNING"] = "1"
        app.launch()
    }

    override func tearDown() async throws { app = nil }

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
        maxOut(slider) // 8 → 24pt
        let gapAfter = reading(beyond: gapBefore + 8) { runtime.frame.minY - title.frame.maxY }
        XCTAssertGreaterThan(
            gapAfter, gapBefore + 8,
            "Widening the section spacing to max barely moved the runtime row "
            + "(gap \(gapBefore) → \(gapAfter)pt) — the sectionSpacing lever isn't wired to FilmCardView."
        )
    }

    /// `maxOut` still reaches the maximum from a slider a drag left one step short of it, as the
    /// first drag of run 41305616620 left the Sekcje slider (47 of 48 steps). Its two retries were
    /// drags from there to the end, and from that close to the end a drag does not move the thumb:
    /// all three read 47 of 48 and the test blamed the lever. The row's own "−" sets that state
    /// exactly, which a drag cannot.
    func testMaxOutRecoversFromADragThatStopsAStepShort() throws {
        let card = firstCard()
        XCTAssertTrue(card.waitForExistence(timeout: 20), "Tuning screen never showed its first card")

        let slider = app.sliders[A11y.Tuning.sectionSpacingSlider]
        XCTAssertTrue(slider.waitForExistence(timeout: 5), "Section-spacing slider missing")
        maxOut(slider)
        let stepDown = app.buttons[A11y.Tuning.stepDown(of: A11y.Tuning.sectionSpacingSlider)]
        XCTAssertTrue(stepDown.waitForExistence(timeout: 5), "Section-spacing slider's − step missing")
        stepDown.tap()
        XCTAssertLessThan(slider.normalizedSliderPosition, 0.99, "the − step left the slider at its maximum")
        maxOut(slider)
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
        maxOut(slider) // 6 → 24pt
        let gapAfter = reading(beyond: gapBefore + 8) { cinema.frame.minY - dayLabel.frame.maxY }
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
        maxOut(slider) // 15 → 24pt
        let heightAfter = reading(beyond: heightBefore + 2) { header.frame.height }
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
        maxOut(slider) // 22 → 34pt
        let heightAfter = reading(beyond: heightBefore + 4) { title.frame.height }
        XCTAssertGreaterThan(
            heightAfter, heightBefore + 4,
            "Maxing the detail-title font barely moved the title height "
            + "(\(heightBefore) → \(heightAfter)pt) — the FilmDetailStyle title lever isn't wired to FilmDetailView."
        )
    }

    /// The viewport/resolution readout lives INSIDE the controls scroll-view
    /// (its first row), not in the pinned header — that's what lets it scroll
    /// away once read instead of permanently occupying header space. Assert it
    /// resolves as a descendant of the controls scroll-view; when it was pinned
    /// in the header it was a sibling of the scroll-view, so this query found
    /// nothing — fail before, pass after.
    func testResolutionReadoutLivesInTheScrollAwayControls() throws {
        let card = firstCard()
        XCTAssertTrue(card.waitForExistence(timeout: 20), "Tuning screen never showed its first card")

        let scrolledReadout = app.scrollViews[A11y.Tuning.controlsScroll]
            .staticTexts[A11y.Tuning.resolutionReadout]
        XCTAssertTrue(
            scrolledReadout.waitForExistence(timeout: 5),
            "Resolution readout isn't inside the controls scroll-view — it's pinned "
            + "in the header instead of sitting in the area that scrolls away."
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

    /// Drags `slider` to its maximum and confirms the slider itself reports it,
    /// re-dragging (up to three times) when it does not. Run 38278405182 read the
    /// Film page's detail title at the same height before and after "maxing" its
    /// font (15 of 15 passed alone locally): the drag was synthesized right after
    /// the controls sheet swapped to that page on a slow runner, and either never
    /// moved the slider or was measured before the title re-laid out — either way
    /// the test blamed the lever's wiring. Checking the slider's
    /// own position separates the two: a drag that cannot land fails here, an
    /// unwired lever still fails the caller's measurement.
    ///
    /// A retry first drags the slider back to its minimum, so every attempt is
    /// a drag along the whole track: a drag that stops a step short of the end
    /// leaves the thumb where a further drag to the end does not move it (run
    /// 41305616620: 47 of 48 steps, all three drags;
    /// testMaxOutRecoversFromADragThatStopsAStepShort).
    private func maxOut(_ slider: XCUIElement) {
        for attempt in 0..<3 {
            if attempt > 0 { slider.adjust(toNormalizedSliderPosition: 0) }
            slider.adjust(toNormalizedSliderPosition: 1.0)
            if slider.normalizedSliderPosition >= 0.99 { return }
        }
        XCTFail("\(slider) stayed at \(slider.normalizedSliderPosition) after three drags to its maximum")
    }

    /// Re-reads `measure` until it passes `threshold` or `timeout` runs out, and
    /// returns the last reading for the caller to assert on: a slider's new value
    /// reaches the measured view only on a later layout pass, which the fixed
    /// 0.4 s sleep this replaced could outrun. An unwired lever still never
    /// passes the threshold, so the caller's assertion still fails.
    private func reading(beyond threshold: CGFloat, timeout: TimeInterval = 5,
                         _ measure: () -> CGFloat) -> CGFloat {
        let deadline = Date().addingTimeInterval(timeout)
        var value = measure()
        while value <= threshold, Date() < deadline {
            Thread.sleep(forTimeInterval: 0.2)
            value = measure()
        }
        return value
    }

    private func firstCard() -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: "\(A11y.Tuning.cardPrefix).0")
            .firstMatch
    }
}

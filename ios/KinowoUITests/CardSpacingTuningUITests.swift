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

    private func firstCard() -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: "\(A11y.Tuning.cardPrefix).0")
            .firstMatch
    }
}

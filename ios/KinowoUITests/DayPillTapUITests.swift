import XCTest

/// Regression guard for "the day pills became unresponsive to taps". The
/// floating top bar sits in a `.safeAreaInset(edge: .top)` over the paged
/// grids, and it has *no* background of its own — the backing is a separate
/// gradient drawn behind it. When the grids were made to ignore the top safe
/// area so they render under the bar (commit 2a3c0df), their vertical scroll
/// gesture started covering the bar region too. So a touch landing on the
/// bar's transparent areas — or a tap arriving while the grid was still
/// coasting — fell through to the scroll view instead of the pills, and the
/// pills felt like they only worked sometimes. (A plain `.tap()` can't catch
/// this: XCUITest waits for the app to go idle before tapping, so the grid has
/// always stopped coasting by then.)
///
/// We reproduce the root cause deterministically: a vertical drag that begins
/// in the top-bar region must NOT scroll the grid — i.e. the grid's pan
/// gesture must not reach up under the bar. With the bug the drag scrolls the
/// grid; with the bar's region clear of the scroll gesture it doesn't.
final class DayPillTapUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
        app.launchArguments += ["-UITests", "1"]
        app.launch()

        // "Wszystkie" so the Films grid has cards regardless of the hour —
        // late in the evening "Dziś" can be empty (see ios uitests-at-night).
        let anytime = app.buttons[A11y.TopBar.datePillAnytime]
        XCTAssertTrue(anytime.waitForExistence(timeout: 15), "Top bar never appeared")
        anytime.tap()

        XCTAssertTrue(firstFilmCard().waitForExistence(timeout: 30), "Grid never appeared")
    }

    override func tearDownWithError() throws {
        app = nil
    }

    /// A vertical drag beginning in the top-bar region must not be picked up by
    /// the grid's scroll gesture. Fails before the fix (the drag scrolls the
    /// grid, because the scroll view extends — gesture and all — under the
    /// translucent bar).
    func testVerticalDragInTopBarDoesNotScrollGrid() throws {
        let card = firstFilmCard()
        XCTAssertTrue(card.waitForExistence(timeout: 5), "No film card")
        let startMinY = card.frame.minY

        // Start in the bar, on the 🎬 brand mark (a non-interactive Text on the
        // far left, so the touch isn't claimed by a pill button), and drag up
        // by a long throw. If the grid's pan reaches here, the cards scroll up.
        let brand = app.staticTexts["🎬"]
        XCTAssertTrue(brand.waitForExistence(timeout: 5), "Brand mark missing")
        let start = brand.coordinate(withNormalizedOffset: CGVector(dx: 0.5, dy: 0.5))
        let end   = start.withOffset(CGVector(dx: 0, dy: -300))
        start.press(forDuration: 0.05, thenDragTo: end, withVelocity: .fast,
                    thenHoldForDuration: 0.0)
        Thread.sleep(forTimeInterval: 1.5) // let any wrongful scroll settle

        let endMinY = firstFilmCard().frame.minY
        XCTAssertEqual(
            endMinY, startMinY, accuracy: 2,
            "A drag starting in the top bar scrolled the grid (\(startMinY) → \(endMinY)): "
            + "the scroll gesture reaches under the bar and steals pill taps")
    }

    /// The pills still do their job: tapping one selects it (becomes the
    /// chosen, accent-filled pill — exposed to XCUITest via the .isSelected
    /// trait).
    func testTappingDayPillSelectsIt() throws {
        let today = app.buttons[A11y.TopBar.datePillToday]
        XCTAssertTrue(today.waitForExistence(timeout: 5), "Day pill missing")

        today.tap()
        let selected = XCTNSPredicateExpectation(
            predicate: NSPredicate(format: "isSelected == true"), object: today)
        XCTAssertEqual(XCTWaiter.wait(for: [selected], timeout: 3), .completed,
                       "Tapping the Dziś pill should select it")
    }

    private func firstFilmCard() -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: A11y.FilmGrid.cell)
            .firstMatch
    }
}

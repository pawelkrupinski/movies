import XCTest

/// Regression guard for the user-reported carousel bug: "when I swipe to another
/// day the cards slide in from the side but then disappear." The cause is a
/// horizontal swipe that STARTS on a film card: `DayCarousel` attaches its drag
/// as a `.simultaneousGesture`, so the card's `NavigationLink` ALSO recognises
/// the swipe — the gesture both changes the day AND pushes that film's detail
/// screen over the grid, so the cards vanish behind a film page the user never
/// meant to open.
///
/// A swipe-to-change-day must change the day, keep us on the listing, and show
/// the new day's cards — never open a film. This drives a real left-swipe that
/// begins on an on-screen card and asserts all three.
///
/// Driven warm (`KINOWO_UITEST_FIXTURE`) with a today+tomorrow fixture so both
/// the start day and the swiped-to day carry cards.
final class DaySwipeCardsVisibleUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
        app.launchArguments += ["-UITests", "1"]
        app.launchEnvironment["KINOWO_UITEST_FIXTURE"] = "1"
        app.launchEnvironment["KINOWO_CLEAR_CITY"] = "1"
        app.launchEnvironment["KINOWO_FORCE_DETECTED_CITY"] = "warszawa"
        app.launch()

        let confirm = app.buttons[A11y.CityGate.confirmButton]
        XCTAssertTrue(confirm.waitForExistence(timeout: 15), "City-confirm screen never showed")
        confirm.tap()
    }

    override func tearDownWithError() throws {
        app = nil
    }

    /// A left-swipe that starts on a film card must advance the day, stay on the
    /// listing, and leave the new day's cards on screen — it must NOT push that
    /// film's detail screen.
    func testSwipingOnACardChangesDayWithoutOpeningTheFilm() throws {
        XCTAssertTrue(anyCard().waitForExistence(timeout: 30), "Grid never mounted")
        Thread.sleep(forTimeInterval: 1.0)
        let screen = app.windows.firstMatch.frame
        guard let card = onScreenCard(in: screen) else {
            return XCTFail("No on-screen card to swipe from")
        }

        // Swipe left starting on the card, well past the 30% commit threshold.
        let start = card.coordinate(withNormalizedOffset: CGVector(dx: 0.5, dy: 0.5))
        let end = start.withOffset(CGVector(dx: -screen.width * 0.7, dy: 0))
        start.press(forDuration: 0.05, thenDragTo: end)
        Thread.sleep(forTimeInterval: 1.5)

        // 1. Still on the listing (not pushed into a film detail): the day-pill
        //    row is the marker — a pushed detail screen replaces the custom bar.
        let tomorrow = app.buttons[A11y.TopBar.datePillTomorrow]
        XCTAssertTrue(
            tomorrow.isHittable,
            "After swiping on a card we are no longer on the listing — the swipe "
            + "opened a film detail screen instead of just changing the day.")
        // 2. The swipe advanced the day.
        XCTAssertTrue(tomorrow.isSelected, "Swipe-left from Dziś should commit to Jutro")
        // 3. The new day's cards are on screen — not blank, not gone.
        XCTAssertNotNil(onScreenCard(in: screen),
                        "After swiping, the next day's cards are not on screen")
    }

    // MARK: - Helpers

    private func allCards() -> [XCUIElement] {
        app.descendants(matching: .any)
            .matching(identifier: A11y.FilmGrid.cell)
            .allElementsBoundByIndex
    }

    private func anyCard() -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: A11y.FilmGrid.cell)
            .firstMatch
    }

    private func onScreenCard(in screen: CGRect) -> XCUIElement? {
        allCards().first { c in
            let f = c.frame
            guard f.width > 1, f.height > 1 else { return false }
            return f.minX >= screen.minX - 1
                && f.minX < screen.maxX
                && screen.intersects(f)
        }
    }
}

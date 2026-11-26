import XCTest

/// Regression guard: a horizontal swipe that STARTS on a film card must change
/// the day without opening that film. With the native paged `TabView` the page
/// scroll view's pan gesture claims the horizontal drag before the card's
/// `NavigationLink` can read it as a tap — but this test pins that down so a
/// future gesture change (or a return to a `.simultaneousGesture` carousel,
/// which DID double-fire and push the film detail over the grid) can't
/// regress it.
///
/// A swipe-to-change-day must change the day, keep us on the listing, and show
/// the new day's cards — never open a film. This drives a real left-swipe that
/// begins on an on-screen card and asserts all three.
///
/// Driven warm (`KINOWO_UITEST_FIXTURE`) with a today+tomorrow fixture so both
/// the start day and the swiped-to day carry cards.
@MainActor
final class DaySwipeCardsVisibleUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUp() async throws {
        continueAfterFailure = false
        app = XCUIApplication()
        FixtureLaunch.intoGrid(app)
    }

    override func tearDown() async throws {
        app = nil
    }

    /// A left-swipe that starts on a film card must advance the day, stay on the
    /// listing, and leave the new day's cards on screen — it must NOT push that
    /// film's detail screen.
    func testSwipingOnACardChangesDayWithoutOpeningTheFilm() throws {
        XCTAssertTrue(anyCard().waitForExistence(timeout: 30), "Grid never mounted")
        Thread.sleep(forTimeInterval: 1.0)
        let screen = app.windows.firstMatch.frame
        guard let card = rightmostOnScreenCard(in: screen) else {
            return XCTFail("No on-screen card to swipe from")
        }

        // Swipe left starting on the card, 70% of the screen: past the paging
        // threshold by DISTANCE alone, and slowly, so no fling velocity decides
        // it. The rightmost card, so the whole drag stays on screen. Order
        // independence #128: a drag from the left column (x 103) ran to x -178;
        // only its on-screen 26% counted, and when the app's main thread stalled
        // during the drag (a cold CI Simulator: 8.6 s on XCUITest's interruption
        // check) the fling velocity was lost too, so the page snapped back to
        // Dziś. The slow drag makes that worst case every run's case.
        // A `ScreenPoint`, not a card-anchored coordinate: XCUITest re-resolves an
        // element-anchored coordinate on the app's main thread while it plays the
        // touch, and that grid search can stall the drag (CardShareMenuUITests,
        // Order independence #74; see `ScreenPoint`).
        let start = ScreenPoint.of(card, normalizedOffset: CGVector(dx: 0.5, dy: 0.5), in: app)
        let end = start.withOffset(CGVector(dx: -screen.width * 0.7, dy: 0))
        start.press(forDuration: 0.05, thenDragTo: end, withVelocity: XCUIGestureVelocity(150),
                    thenHoldForDuration: 0)

        let tomorrow = app.buttons[A11y.TopBar.datePillTomorrow]
        let committed = XCTWaiter.wait(for: [XCTNSPredicateExpectation(
            predicate: NSPredicate(format: "isSelected == true"), object: tomorrow)], timeout: 10)

        // 1. Still on the listing (not pushed into a film detail): the day-pill
        //    row is the marker — a pushed detail screen replaces the custom bar.
        XCTAssertTrue(
            tomorrow.isHittable,
            "After swiping on a card we are no longer on the listing — the swipe "
            + "opened a film detail screen instead of just changing the day.")
        // 2. The swipe advanced the day.
        XCTAssertTrue(committed == .completed, "Swipe-left from Dziś should commit to Jutro")
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

    private func onScreenCards(in screen: CGRect) -> [XCUIElement] {
        allCards().filter { c in
            let f = c.frame
            guard f.width > 1, f.height > 1 else { return false }
            return f.minX >= screen.minX - 1
                && f.minX < screen.maxX
                && screen.intersects(f)
        }
    }

    private func onScreenCard(in screen: CGRect) -> XCUIElement? {
        onScreenCards(in: screen).first
    }

    /// The on-screen card furthest right, so a leftward drag from its centre
    /// across 70% of the screen ends on screen.
    private func rightmostOnScreenCard(in screen: CGRect) -> XCUIElement? {
        onScreenCards(in: screen).max { $0.frame.midX < $1.frame.midX }
    }
}

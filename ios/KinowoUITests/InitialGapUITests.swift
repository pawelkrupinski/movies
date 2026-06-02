import XCTest

/// Regression guard for the "initial gap collapses on its own" bug. On a
/// normal open the first poster row should rest a clear gap below the frosted
/// top bar; the bug parked it ~7px *under* the bar from the first frame and a
/// pull-to-refresh never restored it.
///
/// Root cause: commit 9971d09 dropped `PinScrollContentInset` to let posters
/// blur through the frosted bar, returning the grids to UIKit's `.automatic`
/// content inset, whose recompute collapsed the resting gap. Commit 2a3c0df
/// fixed it by rendering the grid edge-to-edge under the bar and positioning
/// the first row with an explicit `topInset` instead of the `.automatic`
/// safe-area inset. This test pins that resting position so the collapse
/// can't return.
///
/// `ScrollInsetUITests` can't catch this: it samples the first card only
/// *after* `waitForExistence` (post-collapse) and anchors to the Filtry
/// button, which sits on the row *above* the bar's true bottom edge. This
/// test anchors to a 1pt marker at the real bar bottom (`A11y.TopBar.bottomEdge`).
///
/// Driven warm (`KINOWO_UITEST_FIXTURE=1`): a synthetic fixture is delivered
/// synchronously so the grid mounts at first paint, like a warm cache — no
/// dependence on the live repertoire (which is empty late at night).
final class InitialGapUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
        app.launchArguments += ["-UITests", "1"]
        app.launchEnvironment["KINOWO_UITEST_FIXTURE"] = "1" // warm: grid at first paint
        app.launch()
    }

    override func tearDownWithError() throws {
        app = nil
    }

    /// On a normal open the first poster row must rest a clear gap BELOW the
    /// frosted bar (FilmGridView adds `.padding(.top, 10)`), not tucked under
    /// it — at launch and after a pull-to-refresh (which the bug left
    /// collapsed).
    func testFirstCardRestsBelowBarAtLaunchAndAfterPull() throws {
        let card = firstFilmCard()
        XCTAssertTrue(card.waitForExistence(timeout: 30), "Grid never mounted")
        // The collapse, if present, is there from the first frame and does not
        // self-correct — a short settle is enough.
        Thread.sleep(forTimeInterval: 1.0)
        assertCardBelowBar("at launch")

        let grid = app.scrollViews.firstMatch
        if grid.exists {
            let s = grid.coordinate(withNormalizedOffset: CGVector(dx: 0.5, dy: 0.12))
            let e = grid.coordinate(withNormalizedOffset: CGVector(dx: 0.5, dy: 0.95))
            s.press(forDuration: 0.2, thenDragTo: e, withVelocity: .slow, thenHoldForDuration: 0.3)
        }
        Thread.sleep(forTimeInterval: 4.0)
        assertCardBelowBar("after pull-to-refresh")
    }

    private func assertCardBelowBar(_ context: String) {
        let bar = barBottom()
        let minY = firstFilmCard().frame.minY
        XCTAssertGreaterThanOrEqual(
            minY, bar + 6,
            "\(context): first poster row (minY=\(minY)) sits at/under the frosted "
            + "bar bottom (\(bar)) instead of a gap below it — the resting scroll "
            + "inset collapsed (expected ~\(bar + 10))."
        )
    }

    /// Screen-space Y of the frosted bar's true bottom edge, read from the 1pt
    /// marker `TopBar` pins there — the pills / Filtry button sit on the row
    /// above it, so they'd underestimate the bar by its bottom padding.
    private func barBottom() -> CGFloat {
        let edge = app.descendants(matching: .any)[A11y.TopBar.bottomEdge]
        XCTAssertTrue(edge.waitForExistence(timeout: 5), "Top-bar bottom-edge marker missing")
        return edge.frame.maxY
    }

    private func firstFilmCard() -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: A11y.FilmGrid.cell)
            .firstMatch
    }
}

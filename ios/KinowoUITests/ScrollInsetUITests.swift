import XCTest

/// Regression guard for the "movie cards skip and tuck under the top bar on
/// the first drag" bug. The grids' enclosing `UIScrollView`s defaulted to
/// `contentInsetAdjustmentBehavior == .automatic`; UIKit recomputed the
/// adjusted inset the first time the scroll view was touched, yanking the
/// grid up ~17pt and parking the top row under the floating top bar. The
/// fix pins those scroll views to `.never` (see `PinScrollContentInset`)
/// while the bar reserves its height via `.safeAreaInset(edge: .top)`.
///
/// We reproduce with a slow pull-down at the top (the gesture in the bug
/// report) and assert the top-left card's resting position neither jumps up
/// nor ends up behind the bar. The Filtry button (bottom-right of the bar)
/// stands in for the bar's bottom edge.
final class ScrollInsetUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
        app.launchArguments += ["-UITests", "1"]
        app.launch()

        XCTAssertTrue(firstFilmCard(app).waitForExistence(timeout: 30),
                      "Grid never appeared")
    }

    override func tearDownWithError() throws {
        app = nil
    }

    func testFirstCardRestingPositionIsStableAndBelowBar() throws {
        let grid = app.scrollViews.firstMatch
        XCTAssertTrue(grid.waitForExistence(timeout: 5), "No scroll view")

        let barBottom = topBarBottom()
        let initialMinY = firstFilmCard(app).frame.minY

        // Pull down at the very top, the gesture from the bug report. The
        // regression collapsed the grid's top content inset on this first
        // touch, so the first row crept up under the translucent bar.
        pullToRefresh(grid)
        Thread.sleep(forTimeInterval: 6.0) // let refresh spinner + network reload + settle finish

        let restingMinY = firstFilmCard(app).frame.minY

        // The card must not have jumped UP (lower minY) — that upward skip
        // under the bar is the bug's signature. A still-spinning refresh can
        // legitimately push it DOWN, so this guard is one-sided.
        XCTAssertGreaterThanOrEqual(
            restingMinY, initialMinY - 2,
            "First card skipped up on drag: initial=\(initialMinY) resting=\(restingMinY)"
        )

        // …and at rest the card must sit below the bar, not behind it.
        XCTAssertGreaterThanOrEqual(
            restingMinY, barBottom,
            "First card (minY=\(restingMinY)) is hidden under the top bar (bottom=\(barBottom))"
        )
    }

    /// A slow pull-down from near the top of the grid, long enough to engage
    /// the SwiftUI `.refreshable` spinner, then release.
    private func pullToRefresh(_ grid: XCUIElement) {
        let start = grid.coordinate(withNormalizedOffset: CGVector(dx: 0.5, dy: 0.12))
        let end   = grid.coordinate(withNormalizedOffset: CGVector(dx: 0.5, dy: 0.95))
        start.press(forDuration: 0.2, thenDragTo: end, withVelocity: .slow,
                    thenHoldForDuration: 0.3)
    }

    /// Screen-space y of the TopBar's bottom edge, approximated by the
    /// bottom of the Filtry button which hugs the bar's lower edge.
    private func topBarBottom() -> CGFloat {
        let filtry = app.buttons[A11y.TopBar.filtryButton]
        XCTAssertTrue(filtry.waitForExistence(timeout: 5), "Filtry button missing")
        return filtry.frame.maxY
    }

    private func firstFilmCard(_ app: XCUIApplication) -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: A11y.FilmGrid.cell)
            .firstMatch
    }
}

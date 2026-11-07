import XCTest

/// The app window must cover the whole screen, and the film grid must use its
/// full width. Guards the iPhone Duo's unfolded inner display: an app built
/// against an SDK older than iOS 27.1 runs there in a compatibility frame — a
/// phone-sized window inside a black surround — and so would still show two
/// columns on a screen that holds three. On a regular iPhone in portrait both
/// checks hold trivially; run it on the "iPhone Duo" simulator to cover the fold.
@MainActor
final class WindowFillsScreenUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUp() async throws {
        continueAfterFailure = false
        XCUIDevice.shared.orientation = .portrait
        app = XCUIApplication()
        FixtureLaunch.intoGrid(app, environment: ["KINOWO_UITEST_SEED_POSTER": "1"])

        // "Wszystkie" guarantees a dense, multi-row grid regardless of the hour.
        let anytime = app.buttons[A11y.TopBar.datePillAnytime]
        XCTAssertTrue(anytime.waitForExistence(timeout: 30), "Top bar never appeared")
        anytime.tap()
        XCTAssertTrue(cells().firstMatch.waitForExistence(timeout: 30),
                      "Grid never filled after switching to Wszystkie")
    }

    override func tearDown() async throws {
        app = nil
    }

    func testWindowFillsTheScreenAndTheGridUsesItsWidth() throws {
        // A screenshot's image is sized in points, so it is the screen's bounds.
        // The unfolded iPhone Duo has two screens and `XCUIScreen.main` is the
        // folded cover one, so compare against the screen the window is on:
        // the one whose size matches it, else the main screen.
        let window = app.windows.firstMatch.frame
        let screens = XCUIScreen.screens.map { $0.screenshot().image.size }
        let screen = screens.first { abs($0.width - window.width) < 1 && abs($0.height - window.height) < 1 }
            ?? XCUIScreen.main.screenshot().image.size
        XCTAssertEqual(window.width, screen.width, accuracy: 1,
                       "window \(Int(window.width))pt wide on a \(Int(screen.width))pt screen — running in a compatibility frame?")
        XCTAssertEqual(window.height, screen.height, accuracy: 1,
                       "window \(Int(window.height))pt tall on a \(Int(screen.height))pt screen — running in a compatibility frame?")

        // `GridItem(.adaptive(minimum: 160), spacing: 12)` inside 12 pt side
        // padding fits as many 160 pt columns as the padded width holds.
        let expectedColumns = max(1, Int((window.width - 24 + 12) / (160 + 12)))
        let frames = cells().allElementsBoundByIndex
            .map { $0.frame }
            .filter { $0.width > 1 && $0.height > 1 }
        let topY = frames.map { $0.minY }.min() ?? 0
        let topRow = frames.filter { abs($0.minY - topY) < 8 }.count
        XCTAssertEqual(topRow, expectedColumns,
                       "a \(Int(window.width))pt-wide grid should hold \(expectedColumns) columns")
    }

    /// The iPhone Duo keeps its status bar in an 84 pt column down the screen's
    /// right edge — a one-sided trailing safe-area inset, in portrait, that no
    /// other iPhone has. The grid used to bleed under it as under a landscape
    /// phone's sensor-housing insets, and the top bar scaled itself for that
    /// bled width, overflowed its safe-area slot and dragged the grid 57 pt off
    /// the left edge. In portrait the bar and the grid share the safe-area
    /// width: the grid starts inside the screen and ends where the bar does
    /// (Filtry's right edge, a couple of points apart), neither in the column.
    /// The top bar fits the window by itself (portrait has no horizontal
    /// insets, so the window is the safe area): its leading pill and Filtry lie
    /// inside it. On the unfolded iPhone Duo's 669 pt portrait the bar was
    /// 736 pt wide (the inline search field held its full width beside the
    /// pills), clipping the 🎬 and half of Filtry and dragging the grid 34 pt
    /// off both edges. Run on the "iPhone Duo" simulator unfolded, iPhone 17
    /// and iPad mini.
    func testTopBarFitsTheWindowInPortrait() throws {
        let today = app.buttons[A11y.TopBar.datePillToday]
        let filtry = app.buttons[A11y.TopBar.filtryButton]
        XCTAssertTrue(filtry.waitForExistence(timeout: 10), "Filtry button never appeared")
        let window = app.frame
        XCTAssertGreaterThanOrEqual(today.frame.minX, window.minX + 20,
            "the Dziś pill starts at x=\(Int(today.frame.minX)) — the bar's leading 🎬 is off the \(Int(window.width))pt window's left edge")
        XCTAssertLessThanOrEqual(filtry.frame.maxX, window.maxX,
            "Filtry ends at x=\(Int(filtry.frame.maxX)), past the \(Int(window.width))pt window's right edge")
    }

    func testGridStaysWithinTheTopBarInPortrait() throws {
        let filtry = app.buttons[A11y.TopBar.filtryButton]
        XCTAssertTrue(filtry.waitForExistence(timeout: 10), "Filtry button never appeared")
        let barRight = filtry.frame.maxX
        let frames = cells().allElementsBoundByIndex
            .map { $0.frame }
            .filter { $0.width > 1 && $0.height > 1 }
        XCTAssertFalse(frames.isEmpty, "Grid is empty")
        let gridLeft = frames.map { $0.minX }.min() ?? 0
        let gridRight = frames.map { $0.maxX }.max() ?? 0
        XCTAssertGreaterThanOrEqual(gridLeft, app.frame.minX,
                                    "the grid starts at x=\(Int(gridLeft)), off the screen's left edge")
        XCTAssertEqual(gridRight, barRight, accuracy: 6,
                       "the grid ends at x=\(Int(gridRight)) but the top bar at \(Int(barRight)) — one of them runs under a side system bar")
    }

    private func cells() -> XCUIElementQuery {
        app.descendants(matching: .any).matching(identifier: A11y.FilmGrid.cell)
    }
}

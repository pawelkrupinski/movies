import XCTest

/// The app window must cover the whole screen, and the film grid must use its
/// full width. Guards the iPhone Duo's unfolded inner display: an app built
/// against an SDK older than iOS 27.1 runs there in a compatibility frame — a
/// phone-sized window inside a black surround — and so would still show two
/// columns on a screen that holds three. On a regular iPhone in portrait both
/// checks hold trivially; run it on the "iPhone Duo" simulator to cover the fold.
final class WindowFillsScreenUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
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

    override func tearDownWithError() throws {
        app = nil
    }

    func testWindowFillsTheScreenAndTheGridUsesItsWidth() throws {
        // A screenshot's image is sized in points, so it is the screen's bounds.
        let screen = XCUIScreen.main.screenshot().image.size
        let window = app.windows.firstMatch.frame
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

    private func cells() -> XCUIElementQuery {
        app.descendants(matching: .any).matching(identifier: A11y.FilmGrid.cell)
    }
}

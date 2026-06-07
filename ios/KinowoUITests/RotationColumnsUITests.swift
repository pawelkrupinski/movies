import XCTest

/// Regression guard for "rotate to landscape and back to portrait, the grid
/// stays zoomed-in / wider than the screen" (reported 2026-06-07 with a
/// screenshot: portrait device, but the poster grid spills off both edges and
/// shows a partial third column).
///
/// In portrait the `LazyVGrid` is two columns, so every film card's frame must
/// sit inside the screen's width. After a landscape⇄portrait round-trip a paged
/// `TabView` can keep a stale, landscape-width scroll-view frame, leaving the
/// grid laid out wider than the (portrait) window — cards run off the right edge
/// and the first column clips off the left. We rotate, return to portrait, and
/// assert no card escapes the screen horizontally.
final class RotationColumnsUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        XCUIDevice.shared.orientation = .portrait
        app = XCUIApplication()
        app.launchArguments += ["-UITests", "1"]
        app.launchEnvironment["KINOWO_FORCE_DETECTED_CITY"] = "poznan"
        app.launch()

        // Clear the first-launch city gate deterministically (no CoreLocation).
        let confirm = app.buttons[A11y.CityGate.confirmButton]
        if confirm.waitForExistence(timeout: 10) { confirm.tap() }

        // "Wszystkie" guarantees a dense, multi-row grid regardless of the hour.
        let anytime = app.buttons[A11y.TopBar.datePillAnytime]
        XCTAssertTrue(anytime.waitForExistence(timeout: 30), "Top bar never appeared")
        anytime.tap()
        XCTAssertTrue(firstFilmCard().waitForExistence(timeout: 30),
                      "Grid never filled after switching to Wszystkie")
    }

    override func tearDownWithError() throws {
        XCUIDevice.shared.orientation = .portrait
        app = nil
    }

    func testGridStaysWithinScreenWidthAfterRotatingLandscapeAndBack() throws {
        // Land in landscape, settle, then rotate back to portrait — the
        // sequence the bug was reported under.
        XCUIDevice.shared.orientation = .landscapeLeft
        Thread.sleep(forTimeInterval: 1.5)
        XCUIDevice.shared.orientation = .portrait
        Thread.sleep(forTimeInterval: 1.5)

        let screen = app.frame
        let cells = cellFrames()
        XCTAssertGreaterThanOrEqual(cells.count, 2, "Grid emptied after rotation")

        // Portrait is two columns, so the cards in the first row are the
        // tell-tale: a stale landscape layout puts three-plus there, spilling
        // off the screen.
        let topY = cells.map { $0.minY }.min() ?? 0
        let topRowCount = cells.filter { abs($0.minY - topY) < 8 }.count

        let tol: CGFloat = 2
        let offenders = cells.filter { $0.maxX > screen.maxX + tol || $0.minX < screen.minX - tol }
        if !offenders.isEmpty {
            let shot = XCTAttachment(screenshot: XCUIScreen.main.screenshot())
            shot.name = "rotation-overflow"
            shot.lifetime = .keepAlways
            add(shot)
        }
        XCTAssertTrue(
            offenders.isEmpty,
            """
            After landscape→portrait, \(offenders.count) of \(cells.count) film \
            cards fall outside the \(Int(screen.width))pt-wide screen — the grid \
            is laid out wider than the window (stuck in landscape columns). \
            First offender: \(offenders.first.map { "x=\(Int($0.minX))…\(Int($0.maxX))" } ?? "—"). \
            Top-row cell count=\(topRowCount) (portrait expects 2).
            """
        )

        // Portrait is two columns: the first row must hold exactly two cards.
        XCTAssertEqual(topRowCount, 2,
                       "Portrait grid should show two columns; saw \(topRowCount) cards in the top row")
    }

    // MARK: - helpers

    private func cellFrames() -> [CGRect] {
        app.descendants(matching: .any)
            .matching(identifier: A11y.FilmGrid.cell)
            .allElementsBoundByIndex
            .map { $0.frame }
            .filter { $0.width > 1 && $0.height > 1 }
    }

    private func firstFilmCard() -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: A11y.FilmGrid.cell)
            .firstMatch
    }
}

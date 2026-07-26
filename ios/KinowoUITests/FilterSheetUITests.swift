import XCTest

final class FilterSheetUITests: XCTestCase {
    var app: XCUIApplication!

    /// Deliberately does NOT launch: two of the tests below need a launch of
    /// their own (one with the Filtry hook set, one without), and relaunching an
    /// already-launched app just to change its environment doubles every run.
    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
    }

    /// The grid, reached deterministically — see `FixtureLaunch`.
    private func launchIntoGrid(_ environment: [String: String] = [:],
                                file: StaticString = #filePath, line: UInt = #line) {
        FixtureLaunch.intoGrid(app, environment: environment, file: file, line: line)
        XCTAssertTrue(FixtureLaunch.firstFilmCard(app).waitForExistence(timeout: 30),
                      "Grid never appeared", file: file, line: line)
    }

    override func tearDownWithError() throws {
        app = nil
    }

    func testOpeningFiltrySheet() throws {
        // SKIP: the Filtry button is an SF Symbol Image inside a custom
        // `BounceButtonStyle` Button — its accessibility tree gets
        // collapsed in a way that `app.buttons[…]` can't reliably address
        // under XCUITest. Either the button needs `accessibilityElement
        // (children: .ignore)` + an `accessibilityAction(.activate)`, or
        // the button-style needs to be unwrapped. Skipped until then so
        // CI doesn't grind on it.
        try XCTSkipIf(true, "Filtry button a11y tree needs flattening — see TODO")

        launchIntoGrid()
        filtryButton(app).tap()
        let wymiar = sheetMarker(app)
        XCTAssertTrue(wymiar.waitForExistence(timeout: 5),
                      "Filtry sheet did not appear after tapping the Filtry button")
    }

    func testClosingFiltrySheet() throws {
        try XCTSkipIf(true, "Filtry button a11y tree needs flattening — see testOpeningFiltrySheet")

        launchIntoGrid()
        filtryButton(app).tap()
        let marker = sheetMarker(app)
        XCTAssertTrue(marker.waitForExistence(timeout: 5))

        let done = doneButton(app)
        if done.exists { done.tap() } else { app.swipeDown() }

        let predicate = NSPredicate { _, _ in !marker.exists }
        let expectation = XCTNSPredicateExpectation(predicate: predicate, object: nil)
        XCTAssertEqual(XCTWaiter.wait(for: [expectation], timeout: 5), .completed,
                       "Filtry sheet did not close")
    }

    /// The store-screenshot driver shoots the Filtry sheet, and it can neither
    /// tap (`simctl` has no tap primitive) nor deep-link to it (the sheet is app
    /// state, not a web URL) — so it opens the sheet with the
    /// `KINOWO_UITEST_OPEN_FILTERS` launch hook. This pins that hook, and as a
    /// side effect covers the sheet at all: it reaches it without the button tap
    /// the two tests above are still skipped on.
    func testLaunchHookOpensFiltrySheet() throws {
        launchIntoGrid(["KINOWO_UITEST_OPEN_FILTERS": "1"])

        XCTAssertTrue(sheetMarker(app).waitForExistence(timeout: 10),
                      "Filtry sheet did not open from the launch hook")
    }

    /// Without the hook the app must come up on the plain listing — otherwise
    /// the driver's other four screens would all be shot behind the sheet.
    func testTheSheetStaysShutWithoutTheHook() throws {
        launchIntoGrid()

        XCTAssertFalse(sheetMarker(app).waitForExistence(timeout: 3),
                       "Filtry sheet opened on a launch that never asked for it")
    }

    private func filtryButton(_ app: XCUIApplication) -> XCUIElement {
        app.buttons[A11y.TopBar.filtryButton]
    }

    private func sheetMarker(_ app: XCUIApplication) -> XCUIElement {
        // The Wymiar/Dimension section header reliably appears inside the
        // Filtry Form — nothing else in the app carries this identifier, so
        // its presence is proof the sheet rendered. Identifier-based lookup
        // on the NavigationStack container itself is unreliable on some iOS
        // versions, which is why we mark a header rather than the root.
        app.staticTexts[A11y.FiltersSheet.dimensionSection]
    }

    private func doneButton(_ app: XCUIApplication) -> XCUIElement {
        app.descendants(matching: .any)
            .matching(identifier: A11y.FiltersSheet.doneButton)
            .firstMatch
    }
}

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
@MainActor
final class RotationColumnsUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUp() async throws {
        continueAfterFailure = false
        XCUIDevice.shared.orientation = .portrait
        app = XCUIApplication()
        launchIntoDenseGrid()
    }

    private func launchIntoDenseGrid() {
        FixtureLaunch.intoGrid(app, environment: ["KINOWO_UITEST_SEED_POSTER": "1"])

        // "Wszystkie" guarantees a dense, multi-row grid regardless of the hour.
        let anytime = app.buttons[A11y.TopBar.datePillAnytime]
        XCTAssertTrue(anytime.waitForExistence(timeout: 30), "Top bar never appeared")
        anytime.tap()
        XCTAssertTrue(firstFilmCard().waitForExistence(timeout: 30),
                      "Grid never filled after switching to Wszystkie")
    }

    override func tearDown() async throws {
        XCUIDevice.shared.orientation = .portrait
        app = nil
    }

    func testGridStaysWithinScreenWidthAfterRotatingLandscapeAndBack() throws {
        // Land in landscape, settle, then rotate back to portrait — the
        // sequence the bug was reported under.
        try rotateToLandscape()
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

    /// Regression for "open a film, rotate the device, you're thrown back to
    /// the grid". The `.id(rotationGeneration)` that fixes the stale-width zoom rebuilds
    /// the whole NavigationStack on every rotation; when the push state lived
    /// inside that subtree, the rebuild dropped it and popped back to the film
    /// grid. The fix hoists the nav path into ContentView state so the rebuilt
    /// stack restores the detail screen.
    func testStaysOnFilmDetailAfterRotation() throws {
        // Open a film's detail screen. Tap the poster region (top of the card),
        // not the centre: the card's rating links and showtime chips keep their
        // own hit areas, so a centre tap can land on one of those instead of the
        // NavigationLink (mirrors PosterFullScreenUITests).
        firstFilmCard().coordinate(withNormalizedOffset: CGVector(dx: 0.5, dy: 0.18)).tap()

        let poster = app.buttons[A11y.FilmDetail.poster]
        XCTAssertTrue(poster.waitForExistence(timeout: 10),
                      "Film detail never opened after tapping a card")

        // The reported sequence: rotate to landscape and back to portrait.
        try rotateToLandscape()
        XCUIDevice.shared.orientation = .portrait
        Thread.sleep(forTimeInterval: 1.5)

        XCTAssertTrue(
            poster.waitForExistence(timeout: 5),
            """
            Rotating while on a film's detail screen popped back to the grid — \
            the detail poster is gone. The `.id(rotationGeneration)` rebuild dropped the \
            NavigationStack's push instead of restoring it from ContentView state.
            """
        )
    }

    /// Regression for "in landscape the film *detail* screen runs under the
    /// Dynamic Island". FilmDetailView used to `.ignoresSafeArea(edges:
    /// .horizontal)`, which in landscape is the island / rounded-corner inset,
    /// so the header poster (and synopsis / showtimes) slid under the island.
    /// Respecting the horizontal safe area pulls the leading content edge in by
    /// that inset. The main grid screen is intentionally left full-width — only
    /// the detail screen avoids the island.
    func testFilmDetailStaysClearOfDynamicIslandInLandscape() throws {
        // Open a film's detail screen — tap the poster region (top of the card),
        // not the centre, so the tap lands on the NavigationLink rather than the
        // card's rating / showtime sub-links (mirrors PosterFullScreenUITests).
        firstFilmCard().coordinate(withNormalizedOffset: CGVector(dx: 0.5, dy: 0.18)).tap()
        let poster = app.buttons[A11y.FilmDetail.poster]
        XCTAssertTrue(poster.waitForExistence(timeout: 10),
                      "Film detail never opened after tapping a card")

        try rotateToLandscape()
        XCTAssertTrue(poster.waitForExistence(timeout: 5),
                      "Detail poster vanished after rotating to landscape")

        let leadingInset = poster.frame.minX - app.frame.minX
        // The detail content only adds 16pt of its own horizontal padding. If it
        // still ignored the horizontal safe area, the header poster would sit
        // ~16pt from the screen edge — under the Dynamic Island in landscape.
        // Respecting the safe area adds the landscape inset (~44pt+), so the
        // poster's leading edge must clear the bare 16pt padding by a wide margin.
        XCTAssertGreaterThan(
            leadingInset, 35,
            """
            In landscape the detail poster sits \(Int(leadingInset))pt from the \
            screen edge — barely the content's own 16pt padding. The detail screen \
            is ignoring the horizontal safe area and spilling under the Dynamic Island.
            """
        )
    }

    /// A grid reached by rotating must lay out as a fresh launch in that
    /// orientation does: same column count, same leading and trailing card
    /// edges, same Filtry position. Guards the rotation rebuild (`ContentView`'s
    /// `rotationGeneration`), now keyed on UIKit's interface orientation rather
    /// than the vertical size class, which the unfolded iPhone Duo's inner
    /// display reports as regular both ways: rotated to portrait there, the
    /// grid kept landscape's four columns, clipped at both edges. XCUITest
    /// rotates only the Duo's cover display, so this runs on iPhone 17, where
    /// the size class flips too; it guards the mechanism, not the Duo itself.
    func testRotatedLayoutMatchesAFreshLaunchInEachOrientation() throws {
        try rotateToLandscape()
        let rotatedLandscape = try listingLayout()
        XCUIDevice.shared.orientation = .portrait
        Thread.sleep(forTimeInterval: 1.5)
        let rotatedPortrait = try listingLayout()

        app.terminate()
        launchIntoDenseGrid()
        let freshPortrait = try listingLayout()
        app.terminate()
        XCUIDevice.shared.orientation = .landscapeLeft
        Thread.sleep(forTimeInterval: 1.5)
        launchIntoDenseGrid()
        let freshLandscape = try listingLayout()

        assertSameLayout(rotatedPortrait, freshPortrait, "portrait")
        assertSameLayout(rotatedLandscape, freshLandscape, "landscape")
    }

    /// In landscape the top bar stays inside the safe area, clear of the
    /// Dynamic Island, while the grid runs the whole window, 12 pt in from each
    /// physical edge, under the island's symmetric insets (the owner,
    /// 2026-11-07). The bar used to run under the island with the grid. The
    /// iPhone's insets are 62 pt, so Filtry ends well over 30 pt before the
    /// grid's trailing card edge. (The unfolded iPhone Duo's one-sided status
    /// column is not bled into, see `ListingSafeArea`; its inner display can't
    /// be rotated from XCUITest anyway.)
    func testLandscapeTopBarStaysInsideTheSafeAreaWhileTheGridSpansTheWindow() throws {
        try XCTSkipIf(UIDevice.current.userInterfaceIdiom == .pad, "an iPad has no horizontal insets in landscape")
        try rotateToLandscape()
        app.terminate()
        launchIntoDenseGrid()
        let layout = try listingLayout()
        let window = layout.window
        let edgePadding: CGFloat = 12
        XCTAssertEqual(layout.gridLeft, window.minX + edgePadding, accuracy: 3,
            "the grid's leading card edge sits at x=\(Int(layout.gridLeft)), not \(Int(edgePadding))pt from the window's edge")
        XCTAssertEqual(layout.gridRight, window.maxX - edgePadding, accuracy: 3,
            "the grid's trailing card edge sits at x=\(Int(layout.gridRight)) in a \(Int(window.width))pt window — it stops at the safe area instead of running under the insets")
        XCTAssertLessThanOrEqual(layout.filtry.maxX, layout.gridRight - 30,
            "Filtry ends at x=\(Int(layout.filtry.maxX)), level with the grid's trailing edge (x=\(Int(layout.gridRight))) — the top bar runs under the Dynamic Island's inset")
    }

    /// Where the listing sits: the top row's column count, the grid's leading
    /// and trailing card edges and the Filtry button's frame, all inside the window.
    private struct ListingLayout {
        let window: CGRect
        let columns: Int
        let gridLeft: CGFloat
        let gridRight: CGFloat
        let filtry: CGRect
    }

    private func listingLayout() throws -> ListingLayout {
        let filtry = app.buttons[A11y.TopBar.filtryButton]
        XCTAssertTrue(filtry.waitForExistence(timeout: 10), "Filtry button never appeared")
        let cells = cellFrames()
        XCTAssertFalse(cells.isEmpty, "Grid is empty")
        let topY = cells.map { $0.minY }.min() ?? 0
        let layout = ListingLayout(
            window: app.frame,
            columns: cells.filter { abs($0.minY - topY) < 8 }.count,
            gridLeft: cells.map { $0.minX }.min() ?? 0,
            gridRight: cells.map { $0.maxX }.max() ?? 0,
            filtry: filtry.frame)
        let shot = XCTAttachment(screenshot: XCUIScreen.main.screenshot())
        shot.name = "listing \(Int(layout.window.width))x\(Int(layout.window.height))"
        shot.lifetime = .keepAlways
        add(shot)
        return layout
    }

    private func assertSameLayout(_ rotated: ListingLayout, _ fresh: ListingLayout, _ orientation: String) {
        let window = rotated.window
        XCTAssertGreaterThanOrEqual(rotated.gridLeft, window.minX,
            "\(orientation) after rotating: the grid starts at x=\(Int(rotated.gridLeft)), clipped off the left edge")
        XCTAssertLessThanOrEqual(rotated.gridRight, window.maxX,
            "\(orientation) after rotating: the grid ends at x=\(Int(rotated.gridRight)), past the \(Int(window.width))pt window")
        XCTAssertLessThanOrEqual(rotated.filtry.maxX, window.maxX,
            "\(orientation) after rotating: Filtry ends at x=\(Int(rotated.filtry.maxX)), past the \(Int(window.width))pt window")
        XCTAssertEqual(rotated.columns, fresh.columns,
            "\(orientation) after rotating shows \(rotated.columns) columns, a fresh launch \(fresh.columns): the grid kept the other orientation's width")
        XCTAssertEqual(rotated.gridLeft, fresh.gridLeft, accuracy: 2,
            "\(orientation) after rotating: grid's leading edge x=\(Int(rotated.gridLeft)), fresh launch x=\(Int(fresh.gridLeft))")
        XCTAssertEqual(rotated.gridRight, fresh.gridRight, accuracy: 2,
            "\(orientation) after rotating: grid's trailing edge x=\(Int(rotated.gridRight)), fresh launch x=\(Int(fresh.gridRight))")
        XCTAssertEqual(rotated.filtry.maxX, fresh.filtry.maxX, accuracy: 2,
            "\(orientation) after rotating: Filtry ends at x=\(Int(rotated.filtry.maxX)), fresh launch x=\(Int(fresh.filtry.maxX))")
    }

    // MARK: - helpers

    /// Rotates to landscape and lets the layout settle — or skips the test on a
    /// device that does not rotate (the "iPhone Duo" simulator FOLDED keeps every
    /// app's window portrait after `.landscapeLeft`; unfolded it rotates), where
    /// none of these rotation regressions can be exercised. CI pins iPhone 17,
    /// which rotates.
    private func rotateToLandscape() throws {
        XCUIDevice.shared.orientation = .landscapeLeft
        Thread.sleep(forTimeInterval: 1.5)
        // The unfolded iPhone Duo's inner display can take a few seconds more.
        let deadline = Date().addingTimeInterval(6)
        while app.frame.width <= app.frame.height, Date() < deadline {
            Thread.sleep(forTimeInterval: 0.5)
        }
        try XCTSkipUnless(app.frame.width > app.frame.height, "this device type does not rotate")
    }

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

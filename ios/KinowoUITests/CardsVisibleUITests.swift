import XCTest

/// Regression guard for "the film cards stopped showing on the main grid".
///
/// Commit c17d6cb9 ("iOS: finger-following day carousel with synced scroll")
/// replaced the paged `TabView` with `DayCarousel` — a `GeometryReader` hosting
/// a 3-pane `HStack` of `FilmGridView`s parked at `offset(x: -width)` so the
/// CENTRE pane is the one visible at rest. The centre pane is the interactive
/// `FilmGridView` (a bare `ScrollView` with only `.frame(width:)`); the two
/// neighbour panes are read-only mirrors forced to `.frame(maxHeight: .infinity)`.
/// Under that combination the CENTRE pane's `ScrollView` collapsed to ~0 height
/// inside the `HStack`, so at rest the grid was blank — only the off-screen
/// mirror panes still carried cards.
///
/// `RepertoireLaunchUITests`/`InitialGapUITests` proved insufficient: a naive
/// `app.cells.firstMatch` / `…firstMatch` happily grabs a card that is *present*
/// in the accessibility tree but parked off-screen in the previous-day mirror
/// pane (its frame sits at x ≈ -screenWidth). This test pins the stronger
/// invariant the regression actually broke: at launch at least one film card
/// must occupy a real, ON-SCREEN, hittable frame.
///
/// Driven warm (`KINOWO_UITEST_FIXTURE=1`) so a synthetic fixture mounts a dense
/// grid at first paint regardless of the live repertoire / hour.
final class CardsVisibleUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
        app.launchArguments += ["-UITests", "1"]
        app.launchEnvironment["KINOWO_UITEST_FIXTURE"] = "1" // warm: dense grid at first paint
        // Pass the first-launch city gate deterministically so the test stands
        // alone (mirrors DayChangeScrollResetUITests).
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

    /// At least one poster card must land ON SCREEN, occupy a real frame, and be
    /// hittable. The carousel parks the centre pane at `offset(x: -width)`; the
    /// previous-day mirror pane lives at x ≈ -screenWidth, so its cards are in
    /// the tree but off the left edge. We deliberately ignore those and require
    /// a card whose frame intersects the on-screen region — the exact thing the
    /// collapsed centre pane stopped producing.
    func testAtLeastOneCardIsVisibleAndHittableAtLaunch() throws {
        // Wait until *some* card mounts (any pane), then look for an on-screen one.
        XCTAssertTrue(
            anyCard().waitForExistence(timeout: 30),
            "Grid never mounted — no film card in the tree at all")
        // Let the first-frame carousel layout settle.
        Thread.sleep(forTimeInterval: 1.5)

        let screen = app.windows.firstMatch.frame
        let onScreen = onScreenCard(in: screen)

        XCTAssertNotNil(
            onScreen,
            "No film card landed on screen — every card is off-screen (the centre "
            + "carousel pane rendered blank and only the mirror panes carry cards). "
            + "Screen \(screen); card frames: \(allCardFrames())")

        guard let card = onScreen else { return }
        let frame = card.frame
        XCTAssertGreaterThan(frame.width, 40, "On-screen card has near-zero width (\(frame))")
        XCTAssertGreaterThan(frame.height, 40, "On-screen card has near-zero height (\(frame))")
        XCTAssertTrue(
            card.isHittable,
            "On-screen card exists but is not hittable (frame \(frame)) — covered or zero-size")
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

    /// The first card whose frame sits within the visible screen bounds (a real,
    /// non-zero, on-screen frame) — i.e. a card in the CENTRE pane, not an
    /// off-screen mirror.
    private func onScreenCard(in screen: CGRect) -> XCUIElement? {
        allCards().first { c in
            let f = c.frame
            guard f.width > 1, f.height > 1 else { return false }
            // Must overlap the screen horizontally with its origin on-screen:
            // mirror cards sit at minX ≈ -screenWidth (left) or ≈ +screenWidth.
            return f.minX >= screen.minX - 1
                && f.minX < screen.maxX
                && screen.intersects(f)
        }
    }

    private func allCardFrames() -> String {
        allCards().prefix(8).map { "\($0.frame)" }.joined(separator: ", ")
    }
}

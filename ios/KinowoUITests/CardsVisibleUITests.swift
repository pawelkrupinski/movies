import XCTest

/// Regression guard for "the film cards stopped showing on the main grid".
///
/// The day pages live in a native paged `TabView`, which keeps the adjacent
/// day's `FilmGridView` mounted off-screen (a `UIPageViewController` pre-loads
/// its neighbours). So the accessibility tree always carries cards that are NOT
/// on screen — their frames sit at x ≈ ±screenWidth. (The same off-screen-card
/// hazard came up in an even worse form under the short-lived `DayCarousel`,
/// whose collapsed centre pane once rendered blank while only its off-screen
/// mirror panes carried cards.)
///
/// `RepertoireLaunchUITests`/`InitialGapUITests` proved insufficient: a naive
/// `app.cells.firstMatch` / `…firstMatch` happily grabs a card that is *present*
/// in the accessibility tree but parked off-screen in a neighbour day's page.
/// This test pins the stronger invariant: at launch at least one film card must
/// occupy a real, ON-SCREEN, hittable frame.
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
    /// hittable. The paged `TabView` keeps the neighbour day's page mounted at
    /// x ≈ ±screenWidth, so its cards are in the tree but off the edge. We
    /// deliberately ignore those and require a card whose frame intersects the
    /// on-screen region — the current day's grid must actually be visible.
    func testAtLeastOneCardIsVisibleAndHittableAtLaunch() throws {
        // Wait until *some* card mounts (any page), then look for an on-screen one.
        XCTAssertTrue(
            anyCard().waitForExistence(timeout: 30),
            "Grid never mounted — no film card in the tree at all")
        // Let the first-frame paged layout settle.
        Thread.sleep(forTimeInterval: 1.5)

        let screen = app.windows.firstMatch.frame
        let onScreen = onScreenCard(in: screen)

        XCTAssertNotNil(
            onScreen,
            "No film card landed on screen — every card is off-screen (the current "
            + "day's page rendered blank and only neighbour pages carry cards). "
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
    /// non-zero, on-screen frame) — i.e. a card on the current day's page, not an
    /// off-screen neighbour page.
    private func onScreenCard(in screen: CGRect) -> XCUIElement? {
        allCards().first { c in
            let f = c.frame
            guard f.width > 1, f.height > 1 else { return false }
            // Must overlap the screen horizontally with its origin on-screen:
            // neighbour-page cards sit at minX ≈ -screenWidth (left) or ≈ +screenWidth.
            return f.minX >= screen.minX - 1
                && f.minX < screen.maxX
                && screen.intersects(f)
        }
    }

    private func allCardFrames() -> String {
        allCards().prefix(8).map { "\($0.frame)" }.joined(separator: ", ")
    }
}

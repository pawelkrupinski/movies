import XCTest

/// End-to-end guard for inbound deep links: a kinowo.fly.dev Universal Link (or
/// kinowo:// link) must land the user on the right screen. The link is injected
/// through the same `handleDeepLink` path a real Universal Link takes, via the
/// `KINOWO_UITEST_DEEPLINK` launch hook, against the deterministic fixture
/// repertoire (films "Film 1"…"Film 12") so the test is fully offline.
///
/// Exercises the cold-launch deferral path specifically: the link fires before
/// the repertoire loads, so the film push waits for `pendingDeepLink` to be
/// replayed once the fixture films land.
final class DeepLinkUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
        app.launchArguments += ["-UITests", "1"]
        app.launchEnvironment["KINOWO_UITEST_FIXTURE"] = "1"
        app.launchEnvironment["KINOWO_CLEAR_CITY"] = "1"
    }

    override func tearDownWithError() throws {
        app = nil
    }

    func testFilmLinkOpensDetailScreen() throws {
        // The link carries the city, so it satisfies the first-launch gate on
        // its own — no CityGate confirm tap needed.
        app.launchEnvironment["KINOWO_UITEST_DEEPLINK"] = "kinowo://warszawa/film?title=Film%203"
        app.launch()

        let poster = app.descendants(matching: .any).matching(identifier: "filmdetail.poster").firstMatch
        XCTAssertTrue(
            poster.waitForExistence(timeout: 30),
            "A film deep link should push the detail screen, but the detail poster never appeared")
    }

    func testCityLinkLandsOnRepertoireWithoutGate() throws {
        // A city-only link satisfies the gate and shows the grid directly —
        // never the city-confirm screen.
        app.launchEnvironment["KINOWO_UITEST_DEEPLINK"] = "https://kinowo.fly.dev/warszawa/"
        app.launch()

        let card = app.descendants(matching: .any).matching(identifier: "filmgrid.cell").firstMatch
        XCTAssertTrue(card.waitForExistence(timeout: 30), "City link should land on the repertoire grid")
        XCTAssertFalse(
            app.buttons["citygate.confirm.button"].exists,
            "A city deep link should bypass the first-launch city-confirm screen")
    }
}

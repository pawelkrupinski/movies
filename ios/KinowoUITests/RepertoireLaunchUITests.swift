import XCTest

/// The app's own network path end to end -- URLSession, the conditional fetch, the JSON decode,
/// the past-showtime pruning, the day pills -- against a captured Warsaw listing the test runner
/// serves itself (`FixtureServer`), never production: this suite read live kinowo.net until
/// 2026-11, and Order independence went red whenever production's grid or the runner's network
/// did (#33 every connect unanswered for 30 s, #52 / #57 / #58 an empty or failed listing). The
/// served JSON decoding as the server writes it today is the iOS LocalServer layer's job
/// (`LocalServerRepertoireTests`, against the fixture server built from the same commit).
@MainActor
final class RepertoireLaunchUITests: XCTestCase {
    var app: XCUIApplication!
    var server: FixtureServer!

    /// The six films of `repertoire-warszawa.json`.
    static let fixtureTitles = ["Nasza rewolucja", "Dziki Koń Dziewięć", "Zobaczysz wszystko",
                                "Najlepsza owsianka na świecie", "Służący", "Naza"]

    override func setUp() async throws {
        continueAfterFailure = false
        app = XCUIApplication()
        let url = try XCTUnwrap(Bundle(for: Self.self).url(forResource: "repertoire-warszawa", withExtension: "json"),
                                "repertoire-warszawa.json is not in the UI-test bundle")
        let listing = RelativeListing.dated(try Data(contentsOf: url), today: Date())
        server = try FixtureServer(routes: ["/warszawa/api/repertoire": listing])
    }

    override func tearDown() async throws {
        app = nil
        server = nil
    }

    // Synchronous, like every XCUITest here: as an `async` test method its failure under
    // `continueAfterFailure = false` never finished (it hung after its tear down, iOS 26.5),
    // which would bury the message below under an execution-time-allowance timeout.
    func testAppLaunches() throws {
        FixtureLaunch.intoCity(app, city: "warszawa", environment: ["KINOWO_UITEST_SERVER": server.baseURL.absoluteString])
        // Tomorrow, never the launch's default "today": today's showings past the current hour
        // are pruned, and from late evening on today has none left. The listing serves a week.
        let tomorrow = app.buttons[A11y.TopBar.datePillTomorrow]
        XCTAssertTrue(tomorrow.waitForExistence(timeout: 30), "The Jutro day pill is missing after launch")
        tomorrow.tap()

        let servedTitle = app.staticTexts.matching(NSPredicate(format: "label IN %@", Self.fixtureTitles)).firstMatch
        XCTAssertTrue(servedTitle.waitForExistence(timeout: 30),
                      "Expected a card of the served listing after launch. \(listingState()) "
                      + "The app asked the fixture server for \(server.requestedPaths).")
        // The films are real ones, so production may list them too: the grid is the served
        // listing only if the app fetched it from here.
        XCTAssertTrue(server.requestedPaths.contains("/warszawa/api/repertoire"),
                      "The grid came from somewhere else: the app asked the fixture server for \(server.requestedPaths).")
    }

    /// A listing the server fails to serve shows the error state with its retry, not a spinner or an
    /// empty grid. A fresh server's port is a deployment the app's disk cache has never seen
    /// (`ConditionalPayloadCache` keys on it), so nothing cached stands in for the failed fetch.
    func testErrorStateWhenTheListingFails() throws {
        let failing = try FixtureServer(routes: [:])
        FixtureLaunch.intoCity(app, city: "warszawa", environment: ["KINOWO_UITEST_SERVER": failing.baseURL.absoluteString])

        XCTAssertTrue(element(A11y.EmptyState.error).waitForExistence(timeout: 30),
                      "Expected the error state after the listing failed. \(listingState()) "
                      + "The app asked the failing server for \(failing.requestedPaths).")
        XCTAssertTrue(failing.requestedPaths.contains("/warszawa/api/repertoire"),
                      "The app never asked the failing server for the listing: \(failing.requestedPaths).")
    }

    /// Which of the listing's other states the app shows where the grid should be.
    private func listingState() -> String {
        let error = element(A11y.EmptyState.error)
        if error.exists {
            let texts = error.staticTexts.allElementsBoundByIndex.map(\.label).joined(separator: " / ")
            return "The app shows its error state (\(texts))."
        }
        if element(A11y.EmptyState.repertoire).exists { return "The app shows the empty listing." }
        if element(A11y.EmptyState.loading).exists { return "The app is still loading the listing." }
        return "The app shows neither the grid nor its loading, empty or error state."
    }

    private func element(_ identifier: String) -> XCUIElement {
        app.descendants(matching: .any).matching(identifier: identifier).firstMatch
    }
}

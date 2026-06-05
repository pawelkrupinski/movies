import XCTest

/// The first-launch city-gate choice buttons must render as tall, full-width
/// primary actions matching the Android picker (`CityChoiceScreen` uses a
/// 56dp `ControlMinHeight`), not the compact ~44pt system controls / List rows
/// iOS shipped before.
///
/// Reaches the manual picker deterministically via two DEBUG launch hooks:
/// `KINOWO_CLEAR_CITY` drops any persisted city so the gate shows, and
/// `KINOWO_SKIP_LOCATION` bypasses CoreLocation so `CityChoiceView` appears
/// without a permission dialog or the 8s resolve timeout.
///
/// Measures the rendered button height: the tall buttons clear the
/// `cityControlMinHeight` (56pt) floor, comfortably above the compact-control
/// height the screen had before — so the assertion fails before the size
/// change and passes after.
final class CityGateButtonSizeUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
        app.launchArguments += ["-UITests", "1"]
        app.launchEnvironment["KINOWO_CLEAR_CITY"] = "1"
        app.launchEnvironment["KINOWO_SKIP_LOCATION"] = "1"
        app.launch()
    }

    override func tearDownWithError() throws { app = nil }

    func testChoiceButtonsAreTall() throws {
        let buttons = app.buttons.matching(identifier: A11y.CityGate.choiceButton)
        let firstButton = buttons.element(boundBy: 0)
        XCTAssertTrue(
            firstButton.waitForExistence(timeout: 10),
            "City-choice screen never showed a tall choice button"
        )

        // One button per served city (Poznań, Wrocław, Warszawa).
        XCTAssertEqual(buttons.count, 3, "Expected one tall button per served city")

        // `cityControlMinHeight` is 56pt; `.borderedProminent` adds its own
        // vertical padding on top, so a correctly-sized button clears ~56pt.
        // The old compact List rows were ~44pt — 52 cleanly separates them.
        let height = firstButton.frame.height
        XCTAssertGreaterThanOrEqual(
            height, 52,
            "City-choice button is only \(height)pt tall — it isn't rendering at "
            + "the Android-parity tall (≥56pt) size."
        )
    }
}

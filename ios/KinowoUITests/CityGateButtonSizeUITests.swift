import XCTest

/// The first-launch city-gate's primary action ("Pokaż repertuar — …" on the
/// location-confirm screen) must render as a comfortably large iOS button
/// (`.controlSize(.large)`), not the compact system default that read as a
/// small link.
///
/// Reaches the confirm screen deterministically via two DEBUG launch hooks:
/// `KINOWO_CLEAR_CITY` drops any persisted city so the gate shows, and
/// `KINOWO_FORCE_DETECTED_CITY` injects a detected city so `CityConfirmView`
/// appears without a CoreLocation permission dialog or the 8s resolve timeout.
///
/// Measures the rendered button height: a `.large` bordered button clears ~48pt,
/// comfortably above the ~34pt compact default the screen had before — so the
/// assertion fails before the size change and passes after.
final class CityGateButtonSizeUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
        app.launchArguments += ["-UITests", "1"]
        app.launchEnvironment["KINOWO_CLEAR_CITY"] = "1"
        app.launchEnvironment["KINOWO_FORCE_DETECTED_CITY"] = "warszawa"
        app.launch()
    }

    override func tearDownWithError() throws { app = nil }

    func testConfirmButtonIsLarge() throws {
        let button = app.buttons[A11y.CityGate.confirmButton]
        XCTAssertTrue(
            button.waitForExistence(timeout: 10),
            "City-confirm screen never showed its primary button"
        )

        // `.controlSize(.large)` renders the bordered button at ~48–50pt; the
        // compact system default is ~34pt — 44 cleanly separates them.
        let height = button.frame.height
        XCTAssertGreaterThanOrEqual(
            height, 44,
            "Confirm button is only \(height)pt tall — it isn't rendering at the "
            + "enlarged .controlSize(.large) size."
        )
    }
}

import XCTest

/// The manual city picker's toolbar "use my location" button — a re-run of
/// the same first-launch location check, on demand, for a visitor who's
/// already reached the picker (either because location found nothing on
/// first launch, or via "choose other city" in Filtry).
///
/// Reaches the picker the same way `CityChoiceSearchUITests` does:
/// `KINOWO_CLEAR_CITY` shows the gate, `KINOWO_FORCE_DETECTED_CITY` injects a
/// first-launch hit so the confirm screen appears, then "choose another city"
/// drops into the manual list. The button's own tap is then driven by the
/// DEBUG seam `CityChoiceView.locate()` reads directly —
/// `KINOWO_FORCE_LOCATE_UNAVAILABLE` (checked first, so it can be armed
/// alongside `KINOWO_FORCE_DETECTED_CITY` without disturbing how the picker
/// was reached) or the same `KINOWO_FORCE_DETECTED_CITY` for a hit — no
/// CoreLocation dialog, no fix timeout, in either case.
final class CityChoiceLocateUITests: XCTestCase {
    var app: XCUIApplication!

    override func tearDownWithError() throws { app = nil }

    func testLocateButtonHitNavigatesToConfirmScreen() throws {
        app = XCUIApplication()
        app.launchArguments += ["-UITests", "1", "-selectedCountryCode", "pl"]
        app.launchEnvironment["KINOWO_CLEAR_CITY"] = "1"
        // First launch also detects Warszawa — reach the manual picker via
        // "choose another city" first, exactly like `CityChoiceSearchUITests`.
        app.launchEnvironment["KINOWO_FORCE_DETECTED_CITY"] = "warszawa"
        app.launch()

        let chooseOther = app.buttons[A11y.CityGate.chooseOtherButton]
        XCTAssertTrue(chooseOther.waitForExistence(timeout: 10))
        chooseOther.tap()

        let locate = app.buttons[A11y.CityGate.locateButton]
        XCTAssertTrue(locate.waitForExistence(timeout: 10), "Manual picker never showed the locate button")
        locate.tap()

        // The button's own resolver read the SAME `KINOWO_FORCE_DETECTED_CITY`
        // (still "warszawa") the first-launch flow already consumed, so it
        // reports the same hit — the confirm screen reappears.
        let confirm = app.buttons[A11y.CityGate.confirmButton]
        XCTAssertTrue(confirm.waitForExistence(timeout: 10), "Locate button didn't offer the confirm screen on a hit")
    }

    func testLocateButtonMissShowsNoNearbyMessage() throws {
        app = XCUIApplication()
        app.launchArguments += ["-UITests", "1", "-selectedCountryCode", "pl"]
        app.launchEnvironment["KINOWO_CLEAR_CITY"] = "1"
        // Reaches the picker via the ordinary first-launch hit + "choose
        // another city" — `CityResolverView` never reads the unavailable
        // flag below, only `CityChoiceView.locate()` does.
        app.launchEnvironment["KINOWO_FORCE_DETECTED_CITY"] = "warszawa"
        app.launchEnvironment["KINOWO_FORCE_LOCATE_UNAVAILABLE"] = "1"
        app.launch()

        let chooseOther = app.buttons[A11y.CityGate.chooseOtherButton]
        XCTAssertTrue(chooseOther.waitForExistence(timeout: 10))
        chooseOther.tap()

        let locate = app.buttons[A11y.CityGate.locateButton]
        XCTAssertTrue(locate.waitForExistence(timeout: 10), "Manual picker never showed the locate button")
        locate.tap()

        let noNearby = app.staticTexts[A11y.CityGate.noNearbyLocateLabel]
        XCTAssertTrue(noNearby.waitForExistence(timeout: 10), "No 'no nearby city' message after a miss")
    }
}

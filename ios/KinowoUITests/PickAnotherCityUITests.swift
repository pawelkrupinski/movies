import XCTest

/// The Filtry sheet's "Choose another city" button replaced the inline
/// Kraj/Miasto pickers: tapping it re-arms the first-launch flow
/// (`awaitExplicitCityPick` + `clearCity`) and closes the sheet, so `CityGate`
/// re-mounts — landing back on `CityConfirmScreen` under `FixtureLaunch`'s
/// forced-detected-city env (the same short-circuit `CityChoiceSearchUITests`
/// routes around), from which "choose another city" reaches the manual
/// `CityChoiceView` — the same picker shown when the app doesn't yet have a
/// city — instead of offering separate country/city dropdowns inside Filtry.
final class PickAnotherCityUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
    }

    override func tearDownWithError() throws { app = nil }

    func testTappingPickAnotherCityReArmsTheGateAndReachesTheManualPicker() throws {
        FixtureLaunch.intoGrid(app, environment: ["KINOWO_UITEST_OPEN_FILTERS": "1"])

        // The button sits near the bottom of the Filtry `Form` — a SwiftUI
        // List/Form lazily materializes rows, so it isn't in the accessibility
        // tree until scrolled into view (unlike `dimensionSection`, near the
        // top, which every other Filtry test anchors on).
        let pickButton = app.buttons[A11y.FiltersSheet.pickAnotherCityButton]
        for _ in 0..<8 where !pickButton.exists {
            app.swipeUp()
        }
        XCTAssertTrue(pickButton.exists,
                      "Filtry never showed the 'Choose another city' button")
        pickButton.tap()

        // Filtry closed and the grid gave way to the gate: the film cells are
        // gone, and (under this fixture's forced-detected-city env) the gate
        // lands back on the confirm screen rather than the grid.
        let confirm = app.buttons[A11y.CityGate.confirmButton]
        XCTAssertTrue(confirm.waitForExistence(timeout: 10),
                      "CityGate never re-armed after 'Choose another city'")
        XCTAssertFalse(pickButton.exists,
                       "Filtry sheet is still on screen after 'Choose another city'")
        XCTAssertFalse(FixtureLaunch.firstFilmCard(app).exists,
                       "The grid is still on screen after 'Choose another city'")

        // From there, "choose another city" is the same manual picker the
        // first-launch gate has always offered.
        app.buttons[A11y.CityGate.chooseOtherButton].tap()
        let picker = app.descendants(matching: .any)
            .matching(identifier: A11y.CityGate.picker)
            .firstMatch
        XCTAssertTrue(picker.waitForExistence(timeout: 10),
                      "CityChoiceView never reappeared after 'Choose another city'")
    }
}

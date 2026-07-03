import XCTest

/// Drives the cinema pill selector under the top bar: the collapsed handle
/// unfolds a horizontally-scrolling pill row, and tapping a pill single-selects
/// it (accent-filled, exposed to XCUITest via the `.isSelected` trait). Run warm
/// (`KINOWO_UITEST_FIXTURE=1`) so the repertoire — hence the cinema universe the
/// bar is built from — is present at first paint regardless of network timing.
final class CinemaPillBarUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUpWithError() throws {
        continueAfterFailure = false
        app = XCUIApplication()
        app.launchArguments += ["-UITests", "1"]
        app.launchEnvironment["KINOWO_UITEST_FIXTURE"] = "1"
        app.launch()

        // The handle only mounts once the repertoire (with cinemas) has loaded.
        let handle = app.buttons[A11y.CinemaBar.handle]
        XCTAssertTrue(handle.waitForExistence(timeout: 20), "Cinema handle never appeared")
    }

    override func tearDownWithError() throws {
        app = nil
    }

    /// Collapsed the pills are absent from the hierarchy; tapping the handle
    /// unfolds them (leading "Wszystkie" plus one per cinema).
    func testTappingHandleRevealsPills() throws {
        let allPill = app.buttons[A11y.CinemaBar.allPill]
        XCTAssertFalse(allPill.exists, "Pills should be hidden while collapsed")

        app.buttons[A11y.CinemaBar.handle].tap()
        XCTAssertTrue(allPill.waitForExistence(timeout: 3), "Pills did not unfold on tap")

        // The fixture's single cinema is named "Kino".
        let cinemaPill = app.buttons["\(A11y.CinemaBar.pillPrefix).Kino"]
        XCTAssertTrue(cinemaPill.waitForExistence(timeout: 3), "Per-cinema pill missing")
    }

    /// Single-select: tapping a cinema pill selects it and clears "Wszystkie";
    /// tapping "Wszystkie" restores the all-cinemas state.
    func testSelectingCinemaPillIsSingleSelect() throws {
        app.buttons[A11y.CinemaBar.handle].tap()

        let allPill = app.buttons[A11y.CinemaBar.allPill]
        let cinemaPill = app.buttons["\(A11y.CinemaBar.pillPrefix).Kino"]
        XCTAssertTrue(cinemaPill.waitForExistence(timeout: 3), "Per-cinema pill missing")

        // Default: Wszystkie is the selected pill.
        XCTAssertTrue(allPill.isSelected, "Wszystkie should be selected by default")

        cinemaPill.tap()
        let cinemaSelected = XCTNSPredicateExpectation(
            predicate: NSPredicate(format: "isSelected == true"), object: cinemaPill)
        XCTAssertEqual(XCTWaiter.wait(for: [cinemaSelected], timeout: 3), .completed,
                       "Tapping a cinema pill should select it")
        XCTAssertFalse(allPill.isSelected, "Selecting a cinema must clear Wszystkie (single-select)")

        allPill.tap()
        let allSelected = XCTNSPredicateExpectation(
            predicate: NSPredicate(format: "isSelected == true"), object: allPill)
        XCTAssertEqual(XCTWaiter.wait(for: [allSelected], timeout: 3), .completed,
                       "Tapping Wszystkie should restore the all-cinemas selection")
    }
}

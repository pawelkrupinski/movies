import XCTest

/// A search looks through every day: typing moves the day pills to "Wszystkie", clearing the
/// search goes back to the day it began on -- unless a day was picked while it ran, which stays.
@MainActor
final class SearchDayUITests: XCTestCase {
    var app: XCUIApplication!

    override func setUp() async throws {
        continueAfterFailure = false
        app = XCUIApplication()
        FixtureLaunch.intoGrid(app)
        XCTAssertTrue(pill(A11y.TopBar.datePillTomorrow).waitForExistence(timeout: 15), "Top bar never appeared")
    }

    override func tearDown() async throws {
        app = nil
    }

    private func pill(_ id: String) -> XCUIElement { app.buttons[id] }

    private func waitSelected(_ id: String, _ message: String) {
        let selected = XCTNSPredicateExpectation(predicate: NSPredicate(format: "isSelected == true"), object: pill(id))
        XCTAssertEqual(XCTWaiter.wait(for: [selected], timeout: 5), .completed, message)
    }

    private func type(_ text: String) {
        let field = app.textFields[A11y.Search.field]
        XCTAssertTrue(field.waitForExistence(timeout: 5), "Search field missing")
        field.tap()
        field.typeText(text)
    }

    func testASearchShowsEveryDayAndClearingGoesBack() throws {
        pill(A11y.TopBar.datePillTomorrow).tap()
        waitSelected(A11y.TopBar.datePillTomorrow, "Jutro should be picked")
        type("straszny")
        waitSelected(A11y.TopBar.datePillAnytime, "a search should move to Wszystkie")
        app.buttons[A11y.Search.clear].tap()
        waitSelected(A11y.TopBar.datePillTomorrow, "clearing should go back to Jutro")
    }

    func testADayPickedDuringTheSearchStaysWhenItIsCleared() throws {
        pill(A11y.TopBar.datePillToday).tap()
        type("straszny")
        waitSelected(A11y.TopBar.datePillAnytime, "a search should move to Wszystkie")
        pill(A11y.TopBar.datePillWeek).tap()
        waitSelected(A11y.TopBar.datePillWeek, "a pick during the search should hold")
        app.buttons[A11y.Search.clear].tap()
        waitSelected(A11y.TopBar.datePillWeek, "clearing should keep the picked day")
    }
}

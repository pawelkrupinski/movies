import XCTest

/// A held showtime pill shows its room in a bubble centred over the pill, but
/// clamped inside the film card so a pill near the card's edge doesn't push the
/// bubble past it. The clamp reads each pill's measured centre in the card's
/// coordinate space; this asserts the rendered result for every pill of a card
/// whose pills span its width: each bubble still covers its own pill (the
/// left-edge pills' bubbles are shifted right to clear the card's edge, not
/// off the pill) and stays short of the neighbouring card. A lost or stale
/// measurement (centre read as 0) fails it: every bubble then shifts right as
/// if its pill sat on the card's left edge, off its pill and, for the right
/// pills, into the next card.
///
/// The fixture (`KINOWO_UITEST_ROOM_SHOWTIMES`) gives each pill its own room,
/// so a bubble is matched to its pill by text; `KINOWO_UITEST_PIN_ROOM_TOOLTIPS`
/// starts every pill held, because XCUITest queries only after a press ends.
@MainActor
final class RoomTooltipUITests: XCTestCase {
    var app: XCUIApplication!

    /// (time, room) as `RepertoireStore`'s room fixture serves them.
    private let slots = [
        ("23:56", "Sala 1"),
        ("23:57", "Sala 2"),
        ("23:58", "Sala 3"),
        ("23:59", "Sala 4"),
    ]

    override func setUp() async throws {
        continueAfterFailure = false
        app = XCUIApplication()
        FixtureLaunch.intoGrid(app, environment: [
            "KINOWO_UITEST_ROOM_SHOWTIMES": "1",
            "KINOWO_UITEST_PIN_ROOM_TOOLTIPS": "1",
        ])
        // "Wszystkie" so the grid has cards regardless of the hour (see
        // CardShareMenuUITests).
        let anytime = app.buttons[A11y.TopBar.datePillAnytime]
        XCTAssertTrue(anytime.waitForExistence(timeout: 30), "Top bar never appeared")
        anytime.tap()
        XCTAssertTrue(firstFilmCard().waitForExistence(timeout: 30), "Grid never appeared")
    }

    override func tearDown() async throws { app = nil }

    func testRoomTooltipsSitOverTheirPillsWithinTheCard() throws {
        let card = firstFilmCard()
        let nextCardMinX = filmCards().allElementsBoundByIndex
            .map(\.frame)
            .filter { $0.minY == card.frame.minY && $0.minX > card.frame.minX }
            .map(\.minX)
            .min() ?? app.windows.firstMatch.frame.maxX
        var checked = 0
        for (time, room) in slots {
            let pill = card.staticTexts[time].firstMatch
            guard pill.waitForExistence(timeout: 5) else { continue }
            let tooltip = card
                .descendants(matching: .any)
                .matching(NSPredicate(format: "identifier == %@ AND label == %@",
                                      A11y.FilmCard.roomTooltip, room))
                .firstMatch
            XCTAssertTrue(tooltip.waitForExistence(timeout: 5), "No tooltip for \(room)")
            let bubble = tooltip.frame
            let pillMidX = pill.frame.midX
            XCTAssertTrue(bubble.minX <= pillMidX && pillMidX <= bubble.maxX,
                          "\(room)'s bubble \(bubble) doesn't cover its pill's centre x \(pillMidX)")
            XCTAssertLessThan(bubble.maxX, nextCardMinX,
                              "\(room)'s bubble \(bubble) reaches into the next card at x \(nextCardMinX)")
            checked += 1
        }
        // A left-edge pill and at least one further right, or the clamp went untested.
        XCTAssertGreaterThanOrEqual(checked, 2, "Too few pills shown on the card to test the clamp")
    }

    private func filmCards() -> XCUIElementQuery {
        app.descendants(matching: .any).matching(identifier: A11y.FilmGrid.cell)
    }

    private func firstFilmCard() -> XCUIElement {
        filmCards().firstMatch
    }
}

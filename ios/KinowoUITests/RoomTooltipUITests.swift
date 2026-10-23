import XCTest

/// A held showtime pill shows its room in a bubble centred over the pill, but
/// clamped inside the film card so a pill near the card's edge doesn't push the
/// bubble past it, and wrapped onto more lines when the room is wider than the
/// card. The clamp reads each pill's measured centre in the card's coordinate
/// space; this asserts the rendered result for every pill of a card whose pills
/// span its width: each bubble still covers its own pill (the left-edge pills'
/// bubbles are shifted right to clear the card's edge, not off the pill), lies
/// within the card's frame, and sits above its pill rather than over it. A lost
/// or stale measurement (centre read as 0) fails it: every bubble then shifts
/// right as if its pill sat on the card's left edge, off its pill and, for the
/// right pills, past the card. So does a bubble kept on one line however long
/// its room: the fixture's last room is wider than any phone's card.
///
/// Every device size runs the same assertion, so run it on a narrow phone and a
/// wide one (iPhone 16e and iPhone 17 Pro Max) as well as the CI simulator.
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
        ("23:59", "Sala 4 – Dolby Atmos IMAX Laser"),
    ]

    /// The first card's frame with no bubble showing. XCUITest reports a card's
    /// frame as the union of its elements, so once a bubble spills past the
    /// card the card "grows" to contain it; the bound has to be read first.
    private var cardFrame: CGRect = .zero

    override func setUp() async throws {
        continueAfterFailure = false
        app = XCUIApplication()
        launchIntoGrid(pinningTooltips: false)
        cardFrame = firstFilmCard().frame
        app.terminate()
        app = XCUIApplication()
        launchIntoGrid(pinningTooltips: true)
    }

    private func launchIntoGrid(pinningTooltips: Bool) {
        var environment = ["KINOWO_UITEST_ROOM_SHOWTIMES": "1"]
        if pinningTooltips { environment["KINOWO_UITEST_PIN_ROOM_TOOLTIPS"] = "1" }
        FixtureLaunch.intoGrid(app, environment: environment)
        // "Wszystkie" so the grid has cards regardless of the hour (see
        // CardShareMenuUITests).
        let anytime = app.buttons[A11y.TopBar.datePillAnytime]
        XCTAssertTrue(anytime.waitForExistence(timeout: 30), "Top bar never appeared")
        anytime.tap()
        XCTAssertTrue(firstFilmCard().waitForExistence(timeout: 30), "Grid never appeared")
    }

    override func tearDown() async throws { app = nil }

    func testRoomTooltipsSitOverTheirPillsWithinTheCard() throws {
        let card = cardFrame
        var checked: [String] = []
        for (time, room) in slots {
            let pill = firstFilmCard().staticTexts[time].firstMatch
            guard pill.waitForExistence(timeout: 5) else { continue }
            let tooltip = firstFilmCard()
                .descendants(matching: .any)
                .matching(NSPredicate(format: "identifier == %@ AND label == %@",
                                      A11y.FilmCard.roomTooltip, room))
                .firstMatch
            XCTAssertTrue(tooltip.waitForExistence(timeout: 5), "No tooltip for \(room)")
            let bubble = tooltip.frame
            let pillFrame = pill.frame
            XCTAssertTrue(bubble.minX <= pillFrame.midX && pillFrame.midX <= bubble.maxX,
                          "\(room)'s bubble \(bubble) doesn't cover its pill's centre x \(pillFrame.midX)")
            XCTAssertTrue(bubble.minX >= card.minX && bubble.maxX <= card.maxX,
                          "\(room)'s bubble \(bubble) spills past its card \(card)")
            XCTAssertLessThanOrEqual(bubble.maxY, pillFrame.minY,
                                     "\(room)'s bubble \(bubble) covers its pill \(pillFrame)")
            checked.append(room)
        }
        // A left-edge pill, one further right, and the over-wide room, or the
        // clamp and the wrap went untested.
        XCTAssertGreaterThanOrEqual(checked.count, 2, "Too few pills shown on the card to test the clamp")
        XCTAssertTrue(checked.contains(slots.last!.1), "The over-wide room's pill isn't on the first card")
    }

    private func filmCards() -> XCUIElementQuery {
        app.descendants(matching: .any).matching(identifier: A11y.FilmGrid.cell)
    }

    private func firstFilmCard() -> XCUIElement {
        filmCards().firstMatch
    }
}

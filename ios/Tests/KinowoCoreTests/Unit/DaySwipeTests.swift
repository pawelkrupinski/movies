import XCTest
@testable import KinowoCore

final class DaySwipeTests: XCTestCase {

    func testForwardStepWithinBounds() {
        XCTAssertEqual(wrappedDayIndex(current: 0, delta: 1, count: 4), 1)
        XCTAssertEqual(wrappedDayIndex(current: 2, delta: 1, count: 4), 3)
    }

    func testBackwardStepWithinBounds() {
        XCTAssertEqual(wrappedDayIndex(current: 3, delta: -1, count: 4), 2)
        XCTAssertEqual(wrappedDayIndex(current: 1, delta: -1, count: 4), 0)
    }

    func testForwardWrapsPastLast() {
        // Stepping right off the last pill lands back on the first.
        XCTAssertEqual(wrappedDayIndex(current: 3, delta: 1, count: 4), 0)
    }

    func testBackwardWrapsPastFirst() {
        // Stepping left off the first pill lands on the last.
        XCTAssertEqual(wrappedDayIndex(current: 0, delta: -1, count: 4), 3)
    }

    func testLargerDeltaStillWraps() {
        XCTAssertEqual(wrappedDayIndex(current: 0, delta: 5, count: 4), 1)
        XCTAssertEqual(wrappedDayIndex(current: 0, delta: -5, count: 4), 3)
    }

    func testSingleEntryStaysPut() {
        XCTAssertEqual(wrappedDayIndex(current: 0, delta: 1, count: 1), 0)
        XCTAssertEqual(wrappedDayIndex(current: 0, delta: -1, count: 1), 0)
    }

    func testEmptyListReturnsCurrent() {
        XCTAssertEqual(wrappedDayIndex(current: 0, delta: 1, count: 0), 0)
    }

    // MARK: – DateFilter prev/next presets (carousel neighbours)

    func testNextPresetStepsRightThroughPresets() {
        // presets: [today, tomorrow, week, anytime]
        XCTAssertEqual(DateFilter.today.nextPreset, .tomorrow)
        XCTAssertEqual(DateFilter.tomorrow.nextPreset, .week)
        XCTAssertEqual(DateFilter.week.nextPreset, .anytime)
    }

    func testNextPresetWrapsFromAnytimeToToday() {
        XCTAssertEqual(DateFilter.anytime.nextPreset, .today)
    }

    func testPreviousPresetStepsLeftThroughPresets() {
        XCTAssertEqual(DateFilter.anytime.previousPreset, .week)
        XCTAssertEqual(DateFilter.week.previousPreset, .tomorrow)
        XCTAssertEqual(DateFilter.tomorrow.previousPreset, .today)
    }

    func testPreviousPresetWrapsFromTodayToAnytime() {
        XCTAssertEqual(DateFilter.today.previousPreset, .anytime)
    }

    func testNonPresetFilterStaysPut() {
        // A specific date isn't on the pill row, so the carousel must not
        // navigate it away to a preset.
        let specific = DateFilter.specific("2026-06-08")
        XCTAssertEqual(specific.nextPreset, specific)
        XCTAssertEqual(specific.previousPreset, specific)
    }

    // MARK: – previewDayFilter (live pill highlight during a drag)

    private let viewport: CGFloat = 400          // commit threshold = 120
    private let commitFraction: CGFloat = 0.3

    func testBelowThresholdStaysOnCurrent() {
        // A short drag (under width*commitFraction = 120) wouldn't commit, so
        // the pill must keep highlighting the current day.
        XCTAssertEqual(
            previewDayFilter(dragTranslation: -100, viewportWidth: viewport,
                             current: .today, commitFraction: commitFraction),
            .today
        )
        XCTAssertEqual(
            previewDayFilter(dragTranslation: 100, viewportWidth: viewport,
                             current: .today, commitFraction: commitFraction),
            .today
        )
    }

    func testAtThresholdStaysOnCurrent() {
        // Exactly at the threshold is still "not past" — matches the carousel's
        // `abs(tx) > width*commitFraction` strict inequality.
        XCTAssertEqual(
            previewDayFilter(dragTranslation: -120, viewportWidth: viewport,
                             current: .today, commitFraction: commitFraction),
            .today
        )
    }

    func testPastThresholdDraggingLeftPreviewsNextDay() {
        // Drag left past the threshold → the next day would commit.
        XCTAssertEqual(
            previewDayFilter(dragTranslation: -121, viewportWidth: viewport,
                             current: .today, commitFraction: commitFraction),
            DateFilter.today.nextPreset
        )
    }

    func testPastThresholdDraggingRightPreviewsPreviousDay() {
        // Drag right past the threshold → the previous day would commit.
        XCTAssertEqual(
            previewDayFilter(dragTranslation: 121, viewportWidth: viewport,
                             current: .week, commitFraction: commitFraction),
            DateFilter.week.previousPreset
        )
    }

    func testZeroViewportStaysOnCurrent() {
        // No measured width yet → never preview a flip (avoids a divide-by-zero
        // style flip on an unlaid-out carousel).
        XCTAssertEqual(
            previewDayFilter(dragTranslation: -999, viewportWidth: 0,
                             current: .today, commitFraction: commitFraction),
            .today
        )
    }

    func testPreviewWrapsFromAnytimeLeftToToday() {
        // Dragging left off the last preset wraps to the first, same as commit.
        XCTAssertEqual(
            previewDayFilter(dragTranslation: -200, viewportWidth: viewport,
                             current: .anytime, commitFraction: commitFraction),
            .today
        )
    }

    func testPreviewWrapsFromTodayRightToAnytime() {
        XCTAssertEqual(
            previewDayFilter(dragTranslation: 200, viewportWidth: viewport,
                             current: .today, commitFraction: commitFraction),
            .anytime
        )
    }
}

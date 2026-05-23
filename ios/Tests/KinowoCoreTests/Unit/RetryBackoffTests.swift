import XCTest
@testable import KinowoCore

final class RetryBackoffTests: XCTestCase {

    func testFirstFiveAttemptsTripleEachStep() {
        XCTAssertEqual(RetryBackoff.seconds(forAttempt: 0), 2)
        XCTAssertEqual(RetryBackoff.seconds(forAttempt: 1), 6)
        XCTAssertEqual(RetryBackoff.seconds(forAttempt: 2), 18)
        XCTAssertEqual(RetryBackoff.seconds(forAttempt: 3), 54)
        XCTAssertEqual(RetryBackoff.seconds(forAttempt: 4), 162)
    }

    func testStaysAtCeilingForLaterAttempts() {
        // A poster that's been failing for a while shouldn't ramp the
        // delay past `ceilingSeconds` — that would amount to giving up
        // silently, and a flaky CDN can recover even after long
        // outages. Cap stays for the lifetime of the view.
        XCTAssertEqual(RetryBackoff.seconds(forAttempt: 5),  RetryBackoff.ceilingSeconds)
        XCTAssertEqual(RetryBackoff.seconds(forAttempt: 10), RetryBackoff.ceilingSeconds)
        XCTAssertEqual(RetryBackoff.seconds(forAttempt: 99), RetryBackoff.ceilingSeconds)
    }

    func testNegativeAttemptClampsToFirstDelay() {
        // Defensive: callers shouldn't pass negative attempts, but if
        // they do we land on the initial 2s delay rather than
        // returning something nonsensical.
        XCTAssertEqual(RetryBackoff.seconds(forAttempt: -1), 2)
        XCTAssertEqual(RetryBackoff.seconds(forAttempt: -1000), 2)
    }

    func testFirstThreeRetriesLandInsideTwentySixSeconds() {
        // Pins the design intent: a poster CDN that's intermittently
        // dropping requests gets three attempts inside ~26s, which is
        // still fast enough to catch the transient-failure case. If
        // someone later "calms" the backoff to start at 10s, or moves
        // the multiplier above 3, this fires and forces a
        // conversation.
        let total = RetryBackoff.seconds(forAttempt: 0)
                  + RetryBackoff.seconds(forAttempt: 1)
                  + RetryBackoff.seconds(forAttempt: 2)
        XCTAssertEqual(total, 26)
    }
}

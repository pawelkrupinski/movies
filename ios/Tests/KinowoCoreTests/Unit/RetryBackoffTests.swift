import XCTest
@testable import KinowoCore

final class RetryBackoffTests: XCTestCase {

    func testFirstSixAttemptsExpandExponentially() {
        XCTAssertEqual(RetryBackoff.seconds(forAttempt: 0), 2)
        XCTAssertEqual(RetryBackoff.seconds(forAttempt: 1), 4)
        XCTAssertEqual(RetryBackoff.seconds(forAttempt: 2), 8)
        XCTAssertEqual(RetryBackoff.seconds(forAttempt: 3), 16)
        XCTAssertEqual(RetryBackoff.seconds(forAttempt: 4), 32)
        XCTAssertEqual(RetryBackoff.seconds(forAttempt: 5), 64)
    }

    func testStaysAtCeilingForLaterAttempts() {
        // A poster that's been failing for a while shouldn't ramp the
        // delay past `ceilingSeconds` — that would amount to giving up
        // silently, and a flaky CDN can recover even after long
        // outages. Cap stays for the lifetime of the view.
        XCTAssertEqual(RetryBackoff.seconds(forAttempt: 6),  RetryBackoff.ceilingSeconds)
        XCTAssertEqual(RetryBackoff.seconds(forAttempt: 10), RetryBackoff.ceilingSeconds)
        XCTAssertEqual(RetryBackoff.seconds(forAttempt: 99), RetryBackoff.ceilingSeconds)
    }

    func testNegativeAttemptClampsToFirstDelay() {
        // Defensive: callers shouldn't pass negative attempts, but if
        // they do we land on the initial 2s delay rather than crashing
        // with a negative shift amount.
        XCTAssertEqual(RetryBackoff.seconds(forAttempt: -1), 2)
        XCTAssertEqual(RetryBackoff.seconds(forAttempt: -1000), 2)
    }

    func testFirstThreeRetriesLandInsideFourteenSeconds() {
        // Pins the design intent: a poster CDN that's intermittently
        // dropping requests gets three attempts inside ~14s, which is
        // about as fast as we can retry without looking like abuse.
        // If someone later "calms" the backoff to start at 10s, this
        // test fires and forces a conversation.
        let total = RetryBackoff.seconds(forAttempt: 0)
                  + RetryBackoff.seconds(forAttempt: 1)
                  + RetryBackoff.seconds(forAttempt: 2)
        XCTAssertEqual(total, 14)
    }
}

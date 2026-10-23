import XCTest
import Combine
@testable import KinowoAuth

/// The production debounce clock — the one piece of the language push that
/// runs on wall time. `StateSyncServiceTests` drives the service with
/// `ManualDebounceScheduler`, so no test of its logic waits on a real clock;
/// these pin the clock itself. The delays are tiny and the waits are only
/// hang guards: nothing here asserts how long anything took.
@MainActor
final class MainQueueDebounceSchedulerTests: XCTestCase {

    func testTheActionRunsOnceTheDelayHasElapsed() async {
        let fired = expectation(description: "action fired")
        let token = MainQueueDebounceScheduler().schedule(after: 0.01) { fired.fulfill() }
        await fulfillment(of: [fired], timeout: 30)
        _ = token
    }

    /// Cancelling the token (or releasing it: `StateSyncService` overwrites
    /// `pendingDebounce` on every pick) before the delay runs out drops the
    /// action. A later action scheduled with the same delay firing proves the
    /// cancelled one had its chance.
    func testACancelledActionNeverRuns() async {
        var cancelledFired = false
        let scheduler = MainQueueDebounceScheduler()
        let cancelled = scheduler.schedule(after: 0.01) { cancelledFired = true }
        cancelled.cancel()
        var released: AnyCancellable? = scheduler.schedule(after: 0.01) { cancelledFired = true }
        released = nil
        let later = expectation(description: "later action fired")
        let token = scheduler.schedule(after: 0.01) { later.fulfill() }
        await fulfillment(of: [later], timeout: 30)
        XCTAssertFalse(cancelledFired)
        _ = (token, released)
    }
}

import XCTest
@testable import KinowoAuth

/// The production debounce clock — the one piece of the language push that
/// runs on wall time. `StateSyncServiceTests` drives the service with
/// `ManualDebouncer`, so no test of its logic waits on a real clock; these
/// pin the clock itself. The delays are tiny and the waits are only hang
/// guards: nothing here asserts how long anything took.
@MainActor
final class TaskDebouncerTests: XCTestCase {

    func testTheActionRunsOnceTheDelayHasElapsed() async {
        let fired = expectation(description: "action fired")
        let debouncer = TaskDebouncer()
        debouncer.schedule(after: 0.01) { fired.fulfill() }
        await fulfillment(of: [fired], timeout: 30)
    }

    /// A cancelled action, and one a later `schedule` replaced, never run: the
    /// later action firing proves both had their chance.
    func testACancelledOrReplacedActionNeverRuns() async {
        var droppedFired = false
        let debouncer = TaskDebouncer()
        debouncer.schedule(after: 0.01) { droppedFired = true }
        debouncer.cancel()
        debouncer.schedule(after: 0.01) { droppedFired = true }
        let later = expectation(description: "later action fired")
        debouncer.schedule(after: 0.01) { later.fulfill() }
        await fulfillment(of: [later], timeout: 30)
        XCTAssertFalse(droppedFired)
    }
}

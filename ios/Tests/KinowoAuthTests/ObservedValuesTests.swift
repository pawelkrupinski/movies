import XCTest
@testable import KinowoAuth

/// `observedValues`, the iOS 18 stand-in for `Observations` that
/// `StateSyncService` watches the signed-in user and the selected country with.
@MainActor
final class ObservedValuesTests: XCTestCase {

    func testTheCurrentValueComesFirstThenOnePerChange() async {
        let account = SignedInAccount()
        var values = observedValues { account.user?.email }.makeAsyncIterator()
        let first = await values.next()
        XCTAssertEqual(first, .some(nil))
        account.signIn()
        let second = await values.next()
        XCTAssertEqual(second, "test@test.com")
        account.user = nil
        let third = await values.next()
        XCTAssertEqual(third, .some(nil))
    }

    /// Changes made in one main-actor turn arrive as one value: the latest.
    func testChangesInOneTurnArriveAsTheLatest() async {
        let account = SignedInAccount()
        var values = observedValues { account.user?.email }.makeAsyncIterator()
        _ = await values.next()
        account.signIn()
        account.user = UserProfile(displayName: "Other", email: "other@test.com", avatarUrl: nil, provider: "google")
        let next = await values.next()
        XCTAssertEqual(next, "other@test.com")
    }

    /// Cancelling the task that iterates it ends the iteration (a hang here
    /// times the test out) and the observation: a change after it reads nothing.
    func testCancellingTheIteratingTaskEndsIt() async {
        let account = SignedInAccount()
        var reads = 0
        let values = observedValues { reads += 1; return account.user?.email }
        let iterating = Task { for await _ in values {} }
        await Task.yield()
        iterating.cancel()
        await iterating.value
        await settle()                       // the termination's hop to stop the tracker
        let readsWhenEnded = reads
        account.signIn()
        await settle()                       // where a tracker still armed would read again
        XCTAssertEqual(reads, readsWhenEnded)
        account.user = nil
        await settle()
        XCTAssertEqual(reads, readsWhenEnded)
    }

    /// The control for the test above: while iterated, a change is read.
    func testAChangeIsReadWhileIterated() async {
        let account = SignedInAccount()
        var reads = 0
        var values = observedValues { reads += 1; return account.user?.email }.makeAsyncIterator()
        _ = await values.next()
        let before = reads
        account.signIn()
        _ = await values.next()
        XCTAssertGreaterThan(reads, before)
    }

    /// Lets every main-actor hop already queued run.
    private func settle() async {
        for _ in 0..<20 { await MainActor.run {}; await Task.yield() }
    }
}

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
    /// times the test out).
    func testCancellingTheIteratingTaskEndsIt() async {
        let account = SignedInAccount()
        let values = observedValues { account.user?.email }
        let iterating = Task { for await _ in values {} }
        await Task.yield()
        iterating.cancel()
        await iterating.value
        account.signIn()
    }
}

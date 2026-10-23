import XCTest
import KinowoTestSupport
@testable import KinowoCore
@testable import KinowoNetworking

/// The launch decision: `ClientSupportStore.check` asks `/api/client-support`
/// and raises the update gate only on a definite "below the minimum" answer.
/// Every failure mode — offline, non-2xx, an undecodable body — must leave the
/// app usable: our outage must never lock someone out.
@MainActor
final class ClientSupportStoreTests: XCTestCase {

    private let deployment = URL(string: "https://client-support-test.invalid")!
    private let liveBody = #"{"android":{"minimumVersion":"1.0","storeUrl":"https://play.google.com/store/apps/details?id=net.pawel.kinowo"},"ios":{"minimumVersion":"2.1","storeUrl":"https://apps.apple.com/app/id6792566321"}}"#

    override func tearDown() {
        URLProtocolStub.handler = nil
        super.tearDown()
    }

    private func check(appVersion: String, respond: (@Sendable (URLRequest) -> URLProtocolStub.Response)?) async -> ClientSupportStore {
        URLProtocolStub.handler = respond
        let store = ClientSupportStore(session: URLProtocolStub.session(), appVersion: appVersion)
        await store.check(baseURL: deployment)
        return store
    }

    private func ok(_ body: String) -> @Sendable (URLRequest) -> URLProtocolStub.Response {
        { _ in .init(statusCode: 200, headers: ["Content-Type": "application/json"], body: Data(body.utf8)) }
    }

    func testAnOutdatedVersionRaisesTheGateWithTheStoreURL() async {
        let store = await check(appVersion: "2.0.11", respond: ok(liveBody))
        XCTAssertEqual(store.requiredUpdate, URL(string: "https://apps.apple.com/app/id6792566321"))
        XCTAssertEqual(URLProtocolStub.requestedURLs, [deployment.appendingPathComponent("api/client-support")])
    }

    func testACurrentVersionDoesNotRaiseTheGate() async {
        let store = await check(appVersion: "2.1", respond: ok(liveBody))
        XCTAssertNil(store.requiredUpdate)
    }

    func testANetworkFailureDoesNotRaiseTheGate() async {
        // No handler: the stub fails every request with a URLError.
        let store = await check(appVersion: "0.1", respond: nil)
        XCTAssertNil(store.requiredUpdate)
    }

    func testAServerErrorDoesNotRaiseTheGate() async {
        let body = Data(liveBody.utf8)
        let store = await check(appVersion: "0.1") { _ in .init(statusCode: 503, headers: [:], body: body) }
        XCTAssertNil(store.requiredUpdate)
    }

    func testAnUndecodableBodyDoesNotRaiseTheGate() async {
        let store = await check(appVersion: "0.1", respond: ok("<html>maintenance</html>"))
        XCTAssertNil(store.requiredUpdate)
    }
}

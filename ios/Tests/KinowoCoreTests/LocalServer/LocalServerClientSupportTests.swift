import XCTest
@testable import KinowoCore

/// The update gate's contract against a live render: `/api/client-support`
/// (`ClientSupportController`, web) must decode through the app's own
/// `ClientSupport` with the bare `JSONDecoder()` `ClientSupportStore` uses. A
/// body that stops decoding fails OPEN in the app — nobody is ever gated — so
/// only this layer would notice the gate silently switching off.
final class LocalServerClientSupportTests: LocalServerTestCase {

    func testClientSupportDecodesWithAGatedIosRule() throws {
        let fetched = try fetch(baseURL.appendingPathComponent("api/client-support"))
        let support = try XCTUnwrap(ClientSupport.decode(fetched.data), "body did not decode")
        XCTAssertTrue(support.ios.isGated, "iOS rule should be gated: \(support.ios)")
        XCTAssertNotNil(ClientVersion.parse(try XCTUnwrap(support.ios.minimumVersion)))
        XCTAssertNotNil(support.ios.storeUrl.flatMap(URL.init(string:)))
    }
}

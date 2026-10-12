import XCTest
@testable import KinowoCore

/// The Swift mirror of Scala `models.ClientVersion` (common/…/ClientSupport.scala):
/// the same cases `ClientSupportSpec` pins, so the two gates can't drift.
final class ClientSupportTests: XCTestCase {

    // MARK: - Version parsing + comparison

    func testParsesDottedNumericVersions() {
        XCTAssertEqual(ClientVersion.parse("1.2.3"), [1, 2, 3])
        XCTAssertEqual(ClientVersion.parse(" 2.0 "), [2, 0])
        XCTAssertEqual(ClientVersion.parse("7"), [7])
    }

    func testRejectsNonNumericVersions() {
        XCTAssertNil(ClientVersion.parse(""))
        XCTAssertNil(ClientVersion.parse("1.2-beta"))
        XCTAssertNil(ClientVersion.parse("1..2"))
        XCTAssertNil(ClientVersion.parse("v1.2"))
        XCTAssertNil(ClientVersion.parse("1.2."))
    }

    func testComparesComponentWiseNotAsStrings() {
        XCTAssertEqual(ClientVersion.isAtLeast("1.10", minimum: "1.9"), true)
        XCTAssertEqual(ClientVersion.isAtLeast("1.9", minimum: "1.10"), false)
        XCTAssertEqual(ClientVersion.isAtLeast("2.0.11", minimum: "2.0.2"), true)
    }

    func testPadsMissingComponentsWithZero() {
        XCTAssertEqual(ClientVersion.isAtLeast("1.0", minimum: "1.0.0"), true)
        XCTAssertEqual(ClientVersion.isAtLeast("1.0.0", minimum: "1.0"), true)
        XCTAssertEqual(ClientVersion.isAtLeast("1", minimum: "1.0.1"), false)
    }

    func testIsAtLeastIsNilWhenEitherSideIsUnparseable() {
        XCTAssertNil(ClientVersion.isAtLeast("abc", minimum: "1.0"))
        XCTAssertNil(ClientVersion.isAtLeast("1.0", minimum: "abc"))
    }

    // MARK: - isSupported

    private let gated = ClientSupport.Platform(minimumVersion: "2.1", storeUrl: "https://apps.apple.com/app/id1")

    func testBelowTheMinimumOfAGatedPlatformIsUnsupported() {
        XCTAssertFalse(gated.isSupported(version: "2.0.11"))
    }

    func testAtOrAboveTheMinimumIsSupported() {
        XCTAssertTrue(gated.isSupported(version: "2.1"))
        XCTAssertTrue(gated.isSupported(version: "2.1.0"))
        XCTAssertTrue(gated.isSupported(version: "2.10"))
    }

    func testAnUnparseableClientVersionIsSupported() {
        XCTAssertTrue(gated.isSupported(version: "dev-build"))
    }

    func testAnUngatedPlatformSupportsEveryone() {
        XCTAssertTrue(ClientSupport.Platform(minimumVersion: "9.0", storeUrl: nil).isSupported(version: "1.0"))
        XCTAssertTrue(ClientSupport.Platform(minimumVersion: nil, storeUrl: "https://x").isSupported(version: "1.0"))
        XCTAssertTrue(ClientSupport.Platform(minimumVersion: nil, storeUrl: nil).isSupported(version: "1.0"))
    }

    func testAnUnparseableMinimumIsSupported() {
        XCTAssertTrue(ClientSupport.Platform(minimumVersion: "soon", storeUrl: "https://x").isSupported(version: "1.0"))
    }

    // MARK: - Payload decoding

    func testDecodesTheLiveBody() throws {
        let body = try ClientSupport.decode(Data(Fixtures.load("api_client_support").utf8))
        XCTAssertEqual(body?.ios, ClientSupport.Platform(
            minimumVersion: "1.0", storeUrl: "https://apps.apple.com/app/id6792566321"))
        XCTAssertTrue(body?.ios.isGated == true)
    }

    func testDecodesAnUngatedPlatformsNulls() {
        let json = #"{"android":{"minimumVersion":null,"storeUrl":null},"ios":{"minimumVersion":null,"storeUrl":null}}"#
        let body = ClientSupport.decode(Data(json.utf8))
        XCTAssertEqual(body?.ios, ClientSupport.Platform(minimumVersion: nil, storeUrl: nil))
    }

    func testABodyWithoutAnIosHalfDoesNotDecode() {
        XCTAssertNil(ClientSupport.decode(Data(#"{"android":{"minimumVersion":"1.0","storeUrl":null}}"#.utf8)))
        XCTAssertNil(ClientSupport.decode(Data("<html>".utf8)))
    }

    // MARK: - The launch verdict

    func testUpdateURLIsTheStoreURLOnlyWhenTheClientIsBelowTheMinimum() {
        XCTAssertEqual(gated.updateURL(forVersion: "2.0"), URL(string: "https://apps.apple.com/app/id1"))
        XCTAssertNil(gated.updateURL(forVersion: "2.1"))
        XCTAssertNil(gated.updateURL(forVersion: "garbage"))
    }
}

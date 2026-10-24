import Foundation
import XCTest
@testable import KinowoCore

/// The app's User-Agent names its build and the iOS release it runs on, behind the `KinowoIOS/`
/// prefix the server's client classification matches.
final class KinowoUserAgentTests: XCTestCase {

    private func os(_ major: Int, _ minor: Int, _ patch: Int) -> OperatingSystemVersion {
        OperatingSystemVersion(majorVersion: major, minorVersion: minor, patchVersion: patch)
    }

    func testItNamesTheMarketingVersionAndTheOSVersion() {
        XCTAssertEqual(KinowoUserAgent.value(appVersion: "2.1.3", osVersion: os(18, 2, 1)), "KinowoIOS/2.1.3 (iOS 18.2.1)")
    }

    func testAZeroPatchIsLeftOffAsIOSWritesIt() {
        XCTAssertEqual(KinowoUserAgent.value(appVersion: "2.1", osVersion: os(26, 0, 0)), "KinowoIOS/2.1 (iOS 26.0)")
    }

    func testABundleWithoutAVersionIsZero() {
        XCTAssertEqual(KinowoUserAgent.value(appVersion: nil, osVersion: os(18, 0, 0)), "KinowoIOS/0 (iOS 18.0)")
        XCTAssertEqual(KinowoUserAgent.value(appVersion: "", osVersion: os(18, 0, 0)), "KinowoIOS/0 (iOS 18.0)")
    }

    func testTheLiveValueKeepsThePrefixTheServerMatches() {
        XCTAssertTrue(KinowoUserAgent.value.hasPrefix("KinowoIOS/"), KinowoUserAgent.value)
    }
}

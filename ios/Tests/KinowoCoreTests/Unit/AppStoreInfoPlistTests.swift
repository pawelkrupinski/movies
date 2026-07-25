import XCTest

/// App Store submission requirements that live in the app target's `Info.plist`
/// rather than in code. `KinowoCore` ships no bundle of its own (it's the
/// Foundation-only slice of the app), so these read the file straight out of the
/// repo — the artifact `xcodebuild` copies into `Kinowo.app`.
final class AppStoreInfoPlistTests: XCTestCase {

    private func infoPlist() throws -> [String: Any] {
        // Tests/KinowoCoreTests/Unit/<this file> → ios/ → Kinowo/Info.plist
        let url = URL(fileURLWithPath: #filePath)
            .deletingLastPathComponent()   // Unit
            .deletingLastPathComponent()   // KinowoCoreTests
            .deletingLastPathComponent()   // Tests
            .deletingLastPathComponent()   // ios
            .appendingPathComponent("Kinowo/Info.plist")
        let data = try Data(contentsOf: url)
        let parsed = try PropertyListSerialization.propertyList(from: data, format: nil)
        return try XCTUnwrap(parsed as? [String: Any], "Info.plist is not a dictionary")
    }

    /// Without this key every upload stops on App Store Connect's export-
    /// compliance question. The app speaks only standard HTTPS/TLS, which is
    /// exempt, so declaring `false` is both accurate and unattended.
    func testDeclaresExemptFromExportCompliance() throws {
        let value = try infoPlist()["ITSAppUsesNonExemptEncryption"]
        XCTAssertEqual(value as? Bool, false,
                       "ITSAppUsesNonExemptEncryption must be declared false")
    }

    /// 1.0 ships iPhone AND iPad. Narrowing this after release would strand
    /// anyone who already installed it on an iPad, so the setting is pinned:
    /// dropping the `2` is a decision, not a tidy-up. It also commits us to the
    /// 13" iPad screenshot set App Store Connect requires for an iPad app.
    func testShipsForIPhoneAndIPad() throws {
        let project = URL(fileURLWithPath: #filePath)
            .deletingLastPathComponent().deletingLastPathComponent()
            .deletingLastPathComponent().deletingLastPathComponent()
            .appendingPathComponent("Kinowo.xcodeproj/project.pbxproj")
        let pbxproj = try String(contentsOf: project, encoding: .utf8)
        XCTAssertTrue(pbxproj.contains("TARGETED_DEVICE_FAMILY = \"1,2\""),
                      "the app ships for iPad too — see the 13\" screenshot set in App Store Connect")
        XCTAssertFalse(pbxproj.contains("TARGETED_DEVICE_FAMILY = \"1\";"),
                       "no target may narrow to iPhone-only while we ship an iPad build")
    }

    /// A blanket `NSAllowsArbitraryLoads` invites an App Review question, and
    /// nothing here needs it: the app itself loads only `https://`, while
    /// booking links (Safari) and trailers (`WKWebView`) sit outside ATS
    /// regardless. If a cleartext host ever becomes genuinely necessary, add a
    /// scoped `NSExceptionDomains` entry — not the global switch.
    func testDoesNotDisableAppTransportSecurityWholesale() throws {
        let ats = try infoPlist()["NSAppTransportSecurity"] as? [String: Any] ?? [:]
        XCTAssertNil(ats["NSAllowsArbitraryLoads"],
                     "ATS must not be disabled globally — scope an exception instead")
    }
}

import XCTest

/// App Store submission requirements that live in the app target's `Info.plist`
/// rather than in code. Read off disk via `AppSources` — the artifacts
/// `xcodebuild` copies into `Kinowo.app`. The privacy manifest, the other
/// store-facing file, is covered by `AppStorePrivacyManifestTests`.
final class AppStoreInfoPlistTests: XCTestCase {

    private func infoPlist() throws -> [String: Any] {
        try AppSources.plist("Kinowo/Info.plist")
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
        let pbxproj = try AppSources.projectFile()
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

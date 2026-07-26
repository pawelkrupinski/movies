import XCTest

/// `Kinowo/PrivacyInfo.xcprivacy`. Apple checks this at upload: an undeclared
/// "required reason" API comes back as an ITMS-91053 notice days later, long
/// after the archive that caused it. These read the manifest the build ships and
/// re-derive what it OUGHT to say from the app's own sources, so the failure
/// lands here instead.
final class AppStorePrivacyManifestTests: XCTestCase {

    private func manifest() throws -> [String: Any] {
        try AppSources.plist("Kinowo/PrivacyInfo.xcprivacy")
    }

    private func accessedAPICategories() throws -> [String: [String]] {
        let declared = try manifest()["NSPrivacyAccessedAPITypes"] as? [[String: Any]] ?? []
        return declared.reduce(into: [:]) { result, entry in
            guard let category = entry["NSPrivacyAccessedAPIType"] as? String else { return }
            result[category] = entry["NSPrivacyAccessedAPITypeReasons"] as? [String] ?? []
        }
    }

    /// A manifest sitting in the repo but missing from the Resources build phase
    /// is invisible to Apple — the app ships without one and the ITMS notice
    /// arrives anyway. Assert it's actually copied into the bundle.
    func testManifestIsBundledWithTheApp() throws {
        let pbxproj = try AppSources.projectFile()
        XCTAssertTrue(pbxproj.contains("PrivacyInfo.xcprivacy in Resources"),
                      "PrivacyInfo.xcprivacy must be in the app target's Resources build phase")
    }

    /// No ad networks, no analytics, no IDFA. If tracking ever starts, this flag
    /// and `NSPrivacyTrackingDomains` both have to change — and App Tracking
    /// Transparency becomes mandatory, which is a much bigger decision than a
    /// plist edit.
    func testDeclaresNoTracking() throws {
        XCTAssertEqual(try manifest()["NSPrivacyTracking"] as? Bool, false)
        XCTAssertEqual(try manifest()["NSPrivacyTrackingDomains"] as? [String] ?? [], [],
                       "a tracking domain here contradicts NSPrivacyTracking = false")
    }

    func testDeclaresUserDefaultsWithAnAccessReason() throws {
        let reasons = try accessedAPICategories()["NSPrivacyAccessedAPICategoryUserDefaults"]
        XCTAssertEqual(reasons, ["CA92.1"],
                       "the app reads only its own defaults — CA92.1")
    }

    /// The one that earns its keep. Rather than restating the manifest, this
    /// scans the app's sources for the API families Apple gates behind a reason
    /// and fails when one is used but undeclared. Adding, say, a file-timestamp
    /// read to a cache breaks this test on the commit that introduces it.
    func testEveryRequiredReasonAPIUsedIsDeclared() throws {
        // Marker → the category Apple requires for it. Deliberately partial:
        // these are the families this app could plausibly reach for.
        let gatedAPIs: [(marker: String, category: String)] = [
            ("UserDefaults", "NSPrivacyAccessedAPICategoryUserDefaults"),
            ("contentModificationDate", "NSPrivacyAccessedAPICategoryFileTimestamp"),
            ("attributesOfItem", "NSPrivacyAccessedAPICategoryFileTimestamp"),
            ("creationDate", "NSPrivacyAccessedAPICategoryFileTimestamp"),
            ("volumeAvailableCapacity", "NSPrivacyAccessedAPICategoryDiskSpace"),
            ("systemFreeSize", "NSPrivacyAccessedAPICategoryDiskSpace"),
            ("systemUptime", "NSPrivacyAccessedAPICategorySystemBootTime"),
            ("activeInputModes", "NSPrivacyAccessedAPICategoryActiveKeyboards"),
        ]

        let sources = AppSources.appSwiftFiles()
        XCTAssertFalse(sources.isEmpty, "found no app sources — the path walk is wrong")

        let declared = try accessedAPICategories()
        for source in sources {
            let text = try String(contentsOf: source, encoding: .utf8)
            for api in gatedAPIs where text.contains(api.marker) {
                XCTAssertNotNil(
                    declared[api.category],
                    """
                    \(source.lastPathComponent) uses \(api.marker), which Apple gates behind \
                    \(api.category). Declare it in Kinowo/PrivacyInfo.xcprivacy with the \
                    matching reason code, or the next upload earns an ITMS-91053 notice.
                    """
                )
            }
        }
    }

    /// The manifest and the App Store Connect privacy answers are checked
    /// against each other, so the collected-data list can't quietly drift.
    /// Location is the trap: `LocationCityResolver` resolves against the
    /// on-device catalog, so no coordinate is transmitted and it must NOT be
    /// listed. Should a coordinate ever go into a request, this needs updating
    /// in the same commit.
    func testCollectsOnlyAccountDataAndNeverLocation() throws {
        let collected = try manifest()["NSPrivacyCollectedDataTypes"] as? [[String: Any]] ?? []
        let types = Set(collected.compactMap { $0["NSPrivacyCollectedDataType"] as? String })

        XCTAssertEqual(types, [
            "NSPrivacyCollectedDataTypeEmailAddress",
            "NSPrivacyCollectedDataTypeName",
            "NSPrivacyCollectedDataTypeOtherUserContent",
        ], "collected-data list must match the App Privacy answers in App Store Connect")

        for entry in collected {
            XCTAssertEqual(entry["NSPrivacyCollectedDataTypeTracking"] as? Bool, false,
                           "nothing here is collected for tracking")
            XCTAssertEqual(entry["NSPrivacyCollectedDataTypePurposes"] as? [String] ?? [],
                           ["NSPrivacyCollectedDataTypePurposeAppFunctionality"],
                           "sign-in data exists to make the account work, nothing else")
        }
    }
}

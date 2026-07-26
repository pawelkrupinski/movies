import Foundation

/// The app target's own files, read straight off disk.
///
/// `KinowoCore` is the Foundation-only slice of the app and ships no bundle, so
/// the store-facing specs can't go through `Bundle.module` — they read the very
/// artifacts `xcodebuild` copies into `Kinowo.app`. This is the one place that
/// knows how to walk out of the test tree to `ios/`.
enum AppSources {

    /// `ios/` — Tests/KinowoCoreTests/Support/<this file> is four levels down.
    static let root: URL = URL(fileURLWithPath: #filePath)
        .deletingLastPathComponent()   // Support
        .deletingLastPathComponent()   // KinowoCoreTests
        .deletingLastPathComponent()   // Tests
        .deletingLastPathComponent()   // ios

    static func data(_ relativePath: String) throws -> Data {
        try Data(contentsOf: root.appendingPathComponent(relativePath))
    }

    static func plist(_ relativePath: String) throws -> [String: Any] {
        let parsed = try PropertyListSerialization.propertyList(from: data(relativePath), format: nil)
        guard let dictionary = parsed as? [String: Any] else {
            throw NSError(domain: "AppSources", code: 1,
                          userInfo: [NSLocalizedDescriptionKey: "\(relativePath) is not a dictionary"])
        }
        return dictionary
    }

    static func projectFile() throws -> String {
        try String(contentsOf: root.appendingPathComponent("Kinowo.xcodeproj/project.pbxproj"),
                   encoding: .utf8)
    }

    /// Every `.swift` file under `Kinowo/` — the app target's sources, which is
    /// the scope Apple's required-reason rules apply to.
    static func appSwiftFiles() -> [URL] {
        let appDirectory = root.appendingPathComponent("Kinowo")
        guard let walker = FileManager.default.enumerator(at: appDirectory,
                                                          includingPropertiesForKeys: nil) else { return [] }
        return walker.compactMap { $0 as? URL }.filter { $0.pathExtension == "swift" }
    }
}

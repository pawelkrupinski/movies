import Foundation

/// A fresh directory of a test's own under the temp directory, removed by
/// `discard(_:)` — or, at the latest, when the test process exits.
///
/// The exit sweep is what a test case's stored property needs: XCTest builds
/// one instance per test method up front (and `swift test --parallel` builds
/// every instance in every worker), so a `let directory = ...` made at init
/// exists for instances whose `tearDown` never runs. By 2026-10-29 that had
/// left ~9,000 empty `ConditionalPayloadCache-<UUID>` and `PosterStore-<UUID>`
/// directories in the per-user temp directory.
public enum ScratchDirectory {
    private static let live = LockedValue<Set<URL>>([])

    private static let sweepAtExit: Void = {
        atexit { ScratchDirectory.discardAll() }
    }()

    /// `<temp>/<prefix>-<UUID>`, created.
    public static func make(_ prefix: String) -> URL {
        _ = sweepAtExit
        let directory = FileManager.default.temporaryDirectory
            .appendingPathComponent("\(prefix)-\(UUID().uuidString)", isDirectory: true)
        try? FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
        live.withLock { _ = $0.insert(directory) }
        return directory
    }

    public static func discard(_ directory: URL) {
        try? FileManager.default.removeItem(at: directory)
        live.withLock { _ = $0.remove(directory) }
    }

    private static func discardAll() {
        let remaining = live.withLock { live in
            defer { live.removeAll() }
            return live
        }
        remaining.forEach { try? FileManager.default.removeItem(at: $0) }
    }
}

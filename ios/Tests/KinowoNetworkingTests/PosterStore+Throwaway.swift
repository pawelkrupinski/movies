import Foundation
@testable import KinowoCore

extension PosterStore {
    /// A store over its own fresh temp directory that never touches the
    /// network — for the stores that need one injected but whose test isn't
    /// about posters.
    static func throwaway() -> PosterStore {
        PosterStore(directory: FileManager.default.temporaryDirectory
            .appendingPathComponent("PosterStore-\(UUID().uuidString)", isDirectory: true),
                    fetch: { _ in nil })
    }
}

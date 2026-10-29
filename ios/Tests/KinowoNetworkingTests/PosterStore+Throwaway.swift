import Foundation
import KinowoTestSupport
@testable import KinowoCore

extension PosterStore {
    /// A store over its own fresh temp directory that never touches the
    /// network — for the stores that need one injected but whose test isn't
    /// about posters.
    /// Its directory goes when the test process exits (`ScratchDirectory`).
    static func throwaway() -> PosterStore {
        PosterStore(directory: ScratchDirectory.make("PosterStore"), fetch: { _ in nil })
    }
}

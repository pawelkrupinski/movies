import Foundation

public extension UserDefaults {
    /// An empty `UserDefaults` suite for one test: anything a previous run left
    /// behind is wiped first. Pair with `discardScratch(suiteName:)` in
    /// `tearDown` so the next test starts clean too.
    ///
    /// The suite is this process's own (`<suiteName>.<pid>`): a named suite is
    /// one persistent domain for the whole user account, so two `swift test`
    /// runs at once (two worktrees, two agents) otherwise read and wipe each
    /// other's hides and picks mid-test.
    static func scratch(suiteName: String) -> UserDefaults {
        let name = processScratchName(suiteName)
        let defaults = UserDefaults(suiteName: name)!
        defaults.removePersistentDomain(forName: name)
        return defaults
    }

    func discardScratch(suiteName: String) {
        removePersistentDomain(forName: Self.processScratchName(suiteName))
    }

    private static func processScratchName(_ suiteName: String) -> String {
        "\(suiteName).\(ProcessInfo.processInfo.processIdentifier)"
    }
}

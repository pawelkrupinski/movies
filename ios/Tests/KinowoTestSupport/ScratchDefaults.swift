import Foundation

public extension UserDefaults {
    /// An empty, named `UserDefaults` suite for one test: anything a previous
    /// run left behind is wiped first. Pair with `discardScratch(suiteName:)`
    /// in `tearDown` so the next test starts clean too.
    static func scratch(suiteName: String) -> UserDefaults {
        let defaults = UserDefaults(suiteName: suiteName)!
        defaults.removePersistentDomain(forName: suiteName)
        return defaults
    }

    func discardScratch(suiteName: String) {
        removePersistentDomain(forName: suiteName)
    }
}

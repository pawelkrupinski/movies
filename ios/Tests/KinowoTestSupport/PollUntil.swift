import Foundation

/// Suspends until `condition` holds or `timeout` seconds pass, polling every
/// few milliseconds — the replacement for a fixed `Task.sleep` that only hopes
/// an async effect has landed. Returns whether the condition held, so a test
/// asserts on it (`XCTAssertTrue(await pollUntil { … })`) and a miss reads as
/// a failed expectation rather than a hang.
@MainActor
public func pollUntil(timeout: TimeInterval = 5, _ condition: @MainActor () -> Bool) async -> Bool {
    let deadline = Date().addingTimeInterval(timeout)
    while !condition() {
        if Date() >= deadline { return false }
        try? await Task.sleep(nanoseconds: 5_000_000)
    }
    return true
}

import Foundation
import Synchronization

/// A value behind a lock, for state a test shares with code on other threads:
/// the stub's request log and handler (written on URLSession's loading
/// threads, read from the test's), a completion handler's result.
///
/// A class around `Mutex` (which is `~Copyable`, so it can't be captured by
/// the escaping closures that share it) — Sendable checked by the compiler.
public final class LockedValue<Value: Sendable>: Sendable {
    private let mutex: Mutex<Value>
    public init(_ value: Value) { mutex = Mutex(value) }
    public func withLock<Result: Sendable>(_ body: (inout Value) throws -> Result) rethrows -> Result {
        try mutex.withLock { try body(&$0) }
    }
}

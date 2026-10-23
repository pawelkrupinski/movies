import Foundation

/// A value behind a lock, for state a test shares with code on other threads:
/// the stub's request log and handler (written on URLSession's loading
/// threads, read from the test's), a completion handler's result.
///
/// `@unchecked` because the compiler can't see the lock: every access to
/// `value` goes through `withLock`, under `lock`. (`OSAllocatedUnfairLock` /
/// `Mutex` would prove it, but the first is Darwin-only and the second needs
/// macOS 15 / iOS 18, above this package's floors.)
public final class LockedValue<Value>: @unchecked Sendable {
    private let lock = NSLock()
    private var value: Value
    public init(_ value: Value) { self.value = value }
    public func withLock<Result>(_ body: (inout Value) throws -> Result) rethrows -> Result {
        lock.lock(); defer { lock.unlock() }
        return try body(&value)
    }
}

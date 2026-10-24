import Foundation
import Observation

/// Every value `read` produces as the `@Observable` state it reads changes:
/// the current value first, then one per change, read on the main actor once
/// the change has landed — what `Observations` (iOS 26) does, for the iOS 18
/// floor. Changes made in one main-actor turn arrive as one value, the latest.
/// Iterate it in a `Task`; cancelling the task ends the observation.
@MainActor
func observedValues<Value: Sendable>(_ read: @escaping @MainActor () -> Value) -> AsyncStream<Value> {
    let (values, continuation) = AsyncStream.makeStream(of: Value.self)
    let tracker = ObservationTracker(read: read, continuation: continuation)
    continuation.onTermination = { _ in Task { @MainActor in tracker.stop() } }
    tracker.track()
    return values
}

/// Re-arms `withObservationTracking` after each change, which fires once.
@MainActor
private final class ObservationTracker<Value: Sendable> {
    private let read: @MainActor () -> Value
    private let continuation: AsyncStream<Value>.Continuation
    private var stopped = false

    init(read: @escaping @MainActor () -> Value, continuation: AsyncStream<Value>.Continuation) {
        self.read = read
        self.continuation = continuation
    }

    func track() {
        guard !stopped else { return }
        // `onChange` runs as the change is about to land; the hop reads it once it has.
        let value = withObservationTracking { read() } onChange: { [weak self] in
            Task { @MainActor in self?.track() }
        }
        continuation.yield(value)
    }

    func stop() { stopped = true }
}

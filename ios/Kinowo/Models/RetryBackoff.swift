import Foundation

/// Exponential-backoff timing for the poster-retry loop in
/// `PosterImage` (Views/FilmCardView.swift). Lives in the model
/// layer rather than alongside `PosterImage` itself so the math is
/// unit-testable — SwiftUI views are excluded from `KinowoCore`.
///
/// Sequence: 2, 4, 8, 16, 32, 64 (then 64 forever). The first three
/// retries land inside 14s, which rescues the common transient-CDN
/// case where a single request was filtered but the next succeeds.
/// The 64s ceiling keeps a permanently-failing URL from hammering the
/// CDN at any meaningful rate (~1 request / minute / failed poster).
enum RetryBackoff {
    static let ceilingSeconds: Int = 64

    static func seconds(forAttempt attempt: Int) -> Int {
        let clamped = max(0, min(attempt, 5))
        return 1 << (clamped + 1)
    }
}

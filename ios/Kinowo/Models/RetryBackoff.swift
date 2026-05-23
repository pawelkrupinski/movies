import Foundation

/// Exponential-backoff timing for the poster-retry loop in
/// `PosterImage` (Views/FilmCardView.swift). Lives in the model
/// layer rather than alongside `PosterImage` itself so the math is
/// unit-testable — SwiftUI views are excluded from `KinowoCore`.
///
/// Sequence: 2, 6, 18, 54, 162 (then 162 forever). Multiplier of 3
/// rather than 2 so a persistent failure ramps down to ~1 request
/// every ~3 min within a handful of retries, instead of hammering
/// the CDN once a minute. The first three retries still land inside
/// 26s, which is fast enough to rescue the common transient-CDN
/// case where a single request was filtered but the next succeeds.
enum RetryBackoff {
    static let ceilingSeconds: Int = 162

    static func seconds(forAttempt attempt: Int) -> Int {
        let clamped = max(0, min(attempt, 4))
        // 2 · 3^clamped — explicit loop so we stay in Int and don't
        // pull in Foundation's `pow` just to multiply.
        var value = 2
        for _ in 0..<clamped { value *= 3 }
        return value
    }
}

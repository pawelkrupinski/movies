import Foundation

/// Decides when to surface the "swipe to switch screens" onboarding hint.
///
/// There is no tab bar — Filmy / Kina are reached only by swiping the pager —
/// so first-time users get a once-a-day nudge until they discover the gesture.
/// The rule is shared verbatim with the Android app's `SwipeHint`:
///
///  - never again once the user has swiped at least once, and
///  - otherwise at most once per calendar day.
enum SwipeHint {
    static func shouldShow(hasSwiped: Bool, lastShownDate: String, today: String) -> Bool {
        !hasSwiped && lastShownDate != today
    }

    /// `yyyy-MM-dd` in the given calendar — the granularity of the once-a-day
    /// gate. Built from `DateComponents` rather than a `DateFormatter` so the
    /// key is locale-independent and cheap to compute.
    static func dayKey(_ date: Date, calendar: Calendar = .current) -> String {
        let c = calendar.dateComponents([.year, .month, .day], from: date)
        return String(format: "%04d-%02d-%02d", c.year ?? 0, c.month ?? 0, c.day ?? 0)
    }
}

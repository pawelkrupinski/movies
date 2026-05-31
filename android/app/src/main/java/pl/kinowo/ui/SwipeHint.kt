package pl.kinowo.ui

/**
 * Decides when to surface the "swipe to switch screens" onboarding hint.
 *
 * There is no tab bar — Filmy / Kina are reached only by swiping the pager —
 * so first-time users get a once-a-day nudge until they discover the gesture.
 * The rule (shared verbatim with the iOS app's `SwipeHint`):
 *
 *  - never again once the user has swiped at least once, and
 *  - otherwise at most once per calendar day.
 */
object SwipeHint {
    fun shouldShow(hasSwiped: Boolean, lastShownDate: String, today: String): Boolean =
        !hasSwiped && lastShownDate != today
}

import Foundation

/// One-shot suppressor for the "you're nearer city X" proximity prompt.
///
/// The prompt is re-checked on every foreground (see `ContentView`) so
/// travelling between cities mid-session offers the switch as soon as the app
/// returns. But a Google / Facebook sign-in runs through
/// `ASWebAuthenticationSession`, which briefly backgrounds the app — so
/// returning from the OAuth sheet counts as a foreground and re-fires the
/// check, surfacing the prompt again right after login. `suppressNextCheck()`
/// (called when a sign-in starts) makes `consumeShouldSkip()` skip exactly that
/// one post-login check, leaving every genuine later foreground untouched.
final class CitySwitchSuppressor {
    private var skipNextCheck = false

    /// Skip the next proximity check. Call when a sign-in flow starts.
    func suppressNextCheck() {
        skipNextCheck = true
    }

    /// True at most once after `suppressNextCheck()`; resets itself so only the
    /// single post-login foreground is skipped, not later genuine ones.
    func consumeShouldSkip() -> Bool {
        defer { skipNextCheck = false }
        return skipNextCheck
    }
}

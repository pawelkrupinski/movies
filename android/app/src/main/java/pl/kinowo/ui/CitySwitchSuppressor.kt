package pl.kinowo.ui

/**
 * One-shot suppressor for the "you're nearer city X" proximity prompt.
 *
 * The prompt is re-checked on every `ON_RESUME` (see [KinowoApp]) so travelling
 * between cities mid-session offers the switch as soon as the app returns. But a
 * Google / Facebook sign-in opens a Custom Tab, which backgrounds the app — so
 * returning from the OAuth flow counts as a resume and re-fires the check,
 * surfacing the prompt again right after login. [suppressNextCheck] (called when
 * a sign-in starts) makes [consumeShouldSkip] skip exactly that one post-login
 * check, leaving every genuine later resume untouched.
 */
class CitySwitchSuppressor {
    private var skipNextCheck = false

    /** Skip the next proximity check. Call when a sign-in flow starts. */
    fun suppressNextCheck() {
        skipNextCheck = true
    }

    /** True at most once after [suppressNextCheck]; resets itself so only the
     *  single post-login resume is skipped, not later genuine ones. */
    fun consumeShouldSkip(): Boolean = skipNextCheck.also { skipNextCheck = false }
}

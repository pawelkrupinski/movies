package pl.kinowo.ui.common

import android.content.Intent
import androidx.activity.ComponentActivity
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.Robolectric
import org.robolectric.RobolectricTestRunner
import org.robolectric.Shadows.shadowOf
import org.robolectric.annotation.Config

/**
 * Every outbound cinema link — a showtime's `bookingURL` and a cinema's
 * film-page `cinemaURL` (the "open this film on the cinema's site" link) — is
 * opened with [openUrl], which fires a plain implicit `ACTION_VIEW` intent.
 * That is what lets Android App Links hand the https link to an installed
 * cinema app instead of the browser:
 *
 *  - Multikino verifies `multikino.pl` and claims the `/filmy/` path prefix, so
 *    our link `https://www.multikino.pl/filmy/<slug>` opens the Multikino app.
 *  - Biletyna verifies `biletyna.pl` (every path) and Bilety24 verifies
 *    `bilety24.pl`, so their booking links open the respective app.
 *
 * The handoff only works while the open stays a bare, implicit `ACTION_VIEW`.
 * Wrapping it in a Chrome Custom Tab (as the auth flow does) or pinning a
 * browser component would keep it in the browser and silently break handoff,
 * so this locks that property down. (Cinema City and Helios publish no App
 * Links association file on any host we link to, so they stay in the browser —
 * there is nothing on our side to change for them.)
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class CinemaLinkHandoffTest {

    private fun openedIntent(url: String): Intent {
        // An Activity context, like the Compose `LocalContext` the showtime chip
        // and cinema-page link pass in production (see Showings.kt / DetailScreen.kt).
        val activity = Robolectric.buildActivity(ComponentActivity::class.java).setup().get()
        openUrl(activity, url)
        return shadowOf(activity).nextStartedActivity
    }

    private fun assertImplicitView(url: String) {
        val intent = openedIntent(url)
        assertEquals(Intent.ACTION_VIEW, intent.action)
        assertEquals(url, intent.data?.toString())
        // Implicit (no explicit component / package): the system resolves the
        // handler, so a verified App Link can win and open the cinema app.
        assertNull("must stay implicit so App Links can resolve it", intent.component)
        assertNull(intent.`package`)
    }

    @Test
    fun multikinoFilmPageHandsOffToTheApp() {
        // The cinema film-page link (CinemaShowings.cinemaURL); Multikino claims
        // `/filmy/*`, so this exact path is what reaches the Multikino app.
        assertImplicitView("https://www.multikino.pl/filmy/diabel-ubiera-sie-u-prady-2")
    }

    @Test
    fun biletynaBookingHandsOffToTheApp() {
        assertImplicitView("https://biletyna.pl/poznan/kino-rialto/film/blue-velvet?eid=12345")
    }

    @Test
    fun bilety24BookingHandsOffToTheApp() {
        assertImplicitView("https://www.bilety24.pl/wydarzenie/?id=98765")
    }

    @Test
    fun cinemaLinkOpenIsNotWrappedInACustomTab() {
        // A Custom Tab (CustomTabsIntent) tags the intent with a session extra
        // and keeps it in the browser — App Links never fire. Guard against a
        // future "unify URL opening on Custom Tabs" refactor.
        val intent = openedIntent("https://biletyna.pl/event/42")
        assertFalse(
            "cinema links must not be a Custom Tab, or app handoff breaks",
            intent.hasExtra("android.support.customtabs.extra.SESSION"),
        )
    }

    @Test
    fun blankUrlStartsNothing() {
        val activity = Robolectric.buildActivity(ComponentActivity::class.java).setup().get()
        openUrl(activity, null)
        openUrl(activity, "")
        assertNull(shadowOf(activity).nextStartedActivity)
    }
}

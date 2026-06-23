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
 * A showtime's `bookingURL` is opened with [openUrl], which fires a plain
 * implicit `ACTION_VIEW` intent. That is what lets Android App Links hand the
 * https link off to an installed cinema app: Biletyna verifies `biletyna.pl`
 * (its assetlinks.json claims every path) and Bilety24 verifies `bilety24.pl`,
 * so on those domains the user lands in the native ticketing app instead of the
 * browser. The handoff only works while the open stays a bare, implicit
 * `ACTION_VIEW` — wrapping it in a Chrome Custom Tab (as the auth flow does) or
 * pinning a browser component would keep it in the browser and silently break
 * the handoff. This locks that property down.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class BookingAppLinkTest {

    private fun openedIntent(url: String): Intent {
        // An Activity context, like the Compose `LocalContext` the showtime chip
        // passes in production (see Showings.kt).
        val activity = Robolectric.buildActivity(ComponentActivity::class.java).setup().get()
        openUrl(activity, url)
        return shadowOf(activity).nextStartedActivity
    }

    @Test
    fun biletynaBookingOpensAsImplicitViewIntent() {
        val url = "https://biletyna.pl/poznan/kino-rialto/film/blue-velvet?eid=12345"
        val intent = openedIntent(url)

        assertEquals(Intent.ACTION_VIEW, intent.action)
        assertEquals(url, intent.data?.toString())
        // Implicit (no explicit component / package): the system resolves the
        // handler, so a verified App Link can win and open the cinema app.
        assertNull("must stay implicit so App Links can resolve it", intent.component)
        assertNull(intent.`package`)
    }

    @Test
    fun bilety24BookingOpensAsImplicitViewIntent() {
        val url = "https://www.bilety24.pl/wydarzenie/?id=98765"
        val intent = openedIntent(url)

        assertEquals(Intent.ACTION_VIEW, intent.action)
        assertEquals(url, intent.data?.toString())
        assertNull(intent.component)
        assertNull(intent.`package`)
    }

    @Test
    fun bookingOpenIsNotWrappedInACustomTab() {
        // A Custom Tab (CustomTabsIntent) tags the intent with a session extra
        // and keeps it in the browser — App Links never fire. Guard against a
        // future "unify URL opening on Custom Tabs" refactor.
        val intent = openedIntent("https://biletyna.pl/event/42")
        assertFalse(
            "booking must not be a Custom Tab, or app handoff breaks",
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

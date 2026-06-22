package pl.kinowo.ui.common

import android.content.Intent
import androidx.activity.ComponentActivity
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.Robolectric
import org.robolectric.RobolectricTestRunner
import org.robolectric.Shadows.shadowOf
import org.robolectric.annotation.Config

/**
 * The IMDb rating badge deep-links into the IMDb app. [imdbAppUri] derives the
 * `imdb://title/tt…` scheme from the public web URL, and [openImdb] launches it
 * (falling back to the web page when the app isn't installed). The other rating
 * sources keep [openUrl] — RT/Filmweb open via Android App Links and Metacritic
 * has no app.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class ImdbAppLinkTest {

    @Test
    fun derivesAppSchemeFromWebUrl() {
        assertEquals("imdb://title/tt1234567", imdbAppUri("https://www.imdb.com/title/tt1234567/"))
        assertEquals("imdb://title/tt0111161", imdbAppUri("https://imdb.com/title/tt0111161/?ref_=x"))
    }

    @Test
    fun nullWhenNoTitleId() {
        assertNull(imdbAppUri("https://www.imdb.com/"))
        assertNull(imdbAppUri(null))
        assertNull(imdbAppUri(""))
    }

    @Test
    fun openImdbLaunchesTheAppScheme() {
        // An Activity context, like the Compose `LocalContext` the badge passes
        // in production — starting an activity from the Application context would
        // need FLAG_ACTIVITY_NEW_TASK, which the real call site doesn't require.
        val activity = Robolectric.buildActivity(ComponentActivity::class.java).setup().get()
        openImdb(activity, "https://www.imdb.com/title/tt1234567/")

        val started = shadowOf(activity).nextStartedActivity
        assertEquals(Intent.ACTION_VIEW, started.action)
        assertEquals("imdb://title/tt1234567", started.data?.toString())
    }
}

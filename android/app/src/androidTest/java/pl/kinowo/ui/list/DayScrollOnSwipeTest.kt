package pl.kinowo.ui.list

import android.content.Context
import androidx.activity.ComponentActivity
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.semantics.SemanticsProperties
import androidx.compose.ui.semantics.getOrNull
import androidx.compose.ui.test.hasTestTag
import androidx.compose.ui.test.junit4.createAndroidComposeRule
import androidx.compose.ui.test.onRoot
import androidx.compose.ui.test.performTouchInput
import androidx.test.core.app.ApplicationProvider
import androidx.test.ext.junit.runners.AndroidJUnit4
import kotlinx.coroutines.runBlocking
import okhttp3.OkHttpClient
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import pl.kinowo.auth.AuthRepository
import pl.kinowo.auth.UserStateClient
import pl.kinowo.auth.UserSyncState
import pl.kinowo.data.DetailsRepository
import pl.kinowo.data.JsonListCache
import pl.kinowo.data.RepertoireRepository
import pl.kinowo.data.UserPreferences
import pl.kinowo.model.CinemaShowings
import pl.kinowo.model.DayShowings
import pl.kinowo.model.Film
import pl.kinowo.model.FilmDetails
import pl.kinowo.model.Showtime
import pl.kinowo.net.KinowoApi
import pl.kinowo.net.PersistentCookieJar
import pl.kinowo.net.RepertoireApi
import pl.kinowo.ui.KinowoViewModel
import pl.kinowo.ui.theme.KinowoTheme
import java.time.LocalDate
import java.time.ZoneId
import java.time.format.DateTimeFormatter

/**
 * On-device test against the REAL [ListScreen] + a seeded view-model: a day-swipe
 * must land the new day at the TOP, whether you were at the top or scrolled down.
 * Data is Poznań-like (today 40 films, tomorrow a shorter overlapping day). We
 * read the topmost on-screen card's title + Y; "F-0" at the content-top band means
 * tomorrow's first film is at the top. Reproduces the screen-recording bug where
 * the new day inherited the previous day's scroll and appeared mid-list.
 *
 * Real touch + LazyGrid scroll restoration only happen on a device, so this can't
 * live in the Robolectric suite. Run with `./gradlew app:connectedDebugAndroidTest`.
 */
@RunWith(AndroidJUnit4::class)
class DayScrollOnSwipeTest {

    @get:Rule
    val compose = createAndroidComposeRule<ComponentActivity>()

    private fun seedVm(): KinowoViewModel {
        val ctx = ApplicationProvider.getApplicationContext<Context>()
        val zone = ZoneId.of("Europe/Warsaw")
        val today = LocalDate.now(zone).format(DateTimeFormatter.ISO_DATE)
        val tomorrow = LocalDate.now(zone).plusDays(1).format(DateTimeFormatter.ISO_DATE)
        // The same movie F-i plays on BOTH days (shared keys), but at a different
        // time each day, so the day's sort order differs: today F-0..F-39, tomorrow
        // reversed F-39..F-0. So a film at the top of one day sits mid-list the next.
        fun cs(time: String) = listOf(CinemaShowings("Multikino", showtimes = listOf(Showtime(time))))
        // Today: 40 films. Tomorrow: only the first 7 (a much shorter day, as in
        // Poznań). Times increment so today sorts F-0..F-39, tomorrow F-0..F-6.
        val all = (0 until 40).map { i ->
            val showings = buildList {
                add(DayShowings(today, "label", cs("08:%02d".format(i))))
                if (i < 7) add(DayShowings(tomorrow, "label", cs("08:%02d".format(i))))
            }
            Film(title = "F-$i", posterURL = "https://x/$i.jpg", showings = showings)
        }
        val fakeApi = object : RepertoireApi {
            override suspend fun fetchRepertoire(citySlug: String, ifModifiedSince: String?) =
                KinowoApi.Fetched(all, null, false)
        }
        val repo = RepertoireRepository(fakeApi, JsonListCache(ctx.cacheDir, "rep_probe", Film.serializer()))
        runBlocking { repo.reload("warszawa") }
        val http = OkHttpClient()
        val detailsRepo = DetailsRepository(KinowoApi(client = http), JsonListCache(ctx.cacheDir, "det_probe", FilmDetails.serializer()))
        val authRepo = AuthRepository(http, PersistentCookieJar(ctx))
        val noop = object : UserStateClient {
            override suspend fun fetchState() = UserSyncState(emptySet(), emptySet())
            override suspend fun putState(state: UserSyncState) {}
        }
        return KinowoViewModel(repo, detailsRepo, UserPreferences(ctx), authRepo, noop)
    }

    private fun screenW() = compose.activity.resources.displayMetrics.widthPixels.toFloat()
    private fun screenH() = compose.activity.resources.displayMetrics.heightPixels.toFloat()

    /** Topmost on-screen centre card as "title@topY" — topY is its boundsInRoot.top
     *  (negative = scrolled up off the grid's content edge). */
    private fun topCentreFilm(): String? {
        val w = screenW()
        val node = compose.onAllNodes(hasTestTag(FilmCardTestTag))
            .fetchSemanticsNodes()
            .filter { val b = it.boundsInRoot; b.left >= -1f && b.left < w && b.bottom > 0f && b.top < screenH() }
            .minByOrNull { it.boundsInRoot.top } ?: return null
        val title = node.config.getOrNull(SemanticsProperties.Text)?.firstOrNull()?.text
        return "$title@${node.boundsInRoot.top.toInt()}"
    }

    private fun scrollCentreDown(times: Int) {
        repeat(times) {
            compose.onRoot().performTouchInput {
                val x = screenW() / 2f
                down(Offset(x, screenH() * 0.62f))
                moveTo(Offset(x, screenH() * 0.45f))
                moveTo(Offset(x, screenH() * 0.25f))
                up()
            }
            compose.waitForIdle()
        }
    }

    private fun swipeToNextDay() {
        compose.onRoot().performTouchInput {
            val y = screenH() * 0.42f
            down(Offset(screenW() * 0.92f, y))
            moveTo(Offset(screenW() * 0.5f, y))
            moveTo(Offset(screenW() * 0.08f, y))
            up()
        }
        compose.waitForIdle()
    }

    private fun mountSeeded() {
        val vm = seedVm()
        compose.setContent { KinowoTheme { ListScreen(vm, onOpenFilm = {}) } }
        compose.waitForIdle()
    }

    /** The grid's content-top Y (where a film at the exact top sits). A card whose
     *  top is at this Y is at the top; a smaller Y means it's scrolled up off it. */
    private val contentTopBand = 150..240

    private fun assertAtTopOfTomorrow(label: String) {
        val top = topCentreFilm() ?: error("$label: no centre film found")
        val (title, y) = top.substringBeforeLast('@') to top.substringAfterLast('@').toInt()
        // Tomorrow's earliest film is F-0 (08:00). At the top it's the first card,
        // sitting at the content-top band — not a later film, and not scrolled up.
        assertEquals("$label: should land on tomorrow's first film", "F-0", title)
        assertTrue("$label: top card must sit at the content top (y=$y, got '$top')", y in contentTopBand)
    }

    @Test
    fun swipeFromTheTopLandsAtTheTop() {
        mountSeeded()
        swipeToNextDay()
        assertAtTopOfTomorrow("at-top → swipe")
    }

    @Test
    fun swipeWhileScrolledDownLandsAtTheTopOfTheNewDay() {
        // The bug from the screen recording: scrolled down on today, swipe to
        // tomorrow, and the new day inherited the scroll — it appeared mid-list
        // (card bodies, posters off the top) instead of at the top.
        mountSeeded()
        scrollCentreDown(3)
        swipeToNextDay()
        assertAtTopOfTomorrow("scrolled → swipe")
    }

}

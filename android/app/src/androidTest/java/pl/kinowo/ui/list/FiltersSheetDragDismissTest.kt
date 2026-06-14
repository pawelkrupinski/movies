package pl.kinowo.ui.list

import android.content.Context
import androidx.activity.ComponentActivity
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.rememberModalBottomSheetState
import androidx.compose.ui.geometry.Offset
import androidx.compose.ui.test.hasScrollAction
import androidx.compose.ui.test.hasText
import androidx.compose.ui.test.junit4.createAndroidComposeRule
import androidx.compose.ui.test.onNodeWithText
import androidx.compose.ui.test.performClick
import androidx.compose.ui.test.performScrollToNode
import androidx.compose.ui.test.performTouchInput
import androidx.test.core.app.ApplicationProvider
import androidx.test.ext.junit.runners.AndroidJUnit4
import okhttp3.OkHttpClient
import org.junit.Assert.assertFalse
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
import pl.kinowo.ui.KinowoViewModel
import pl.kinowo.ui.theme.KinowoTheme

/**
 * On-device test for the Filtry [ModalBottomSheet] (FiltersSheet.kt): a downward
 * drag that STARTS while the filter list is scrolled below its top must scroll
 * the list back up — it must NOT drag the sheet closed, even when the list
 * reaches the top mid-gesture and the leftover would otherwise spill into the
 * sheet's own drag-to-close. The sheet may only close when the drag begins with
 * the list already at the top.
 *
 * Real touch + anchoredDraggable settle physics only happen on a device, so this
 * lives in the instrumented suite (mirrors DayScrollOnSwipeTest). Run with
 * `./gradlew app:connectedDebugAndroidTest`.
 */
@RunWith(AndroidJUnit4::class)
class FiltersSheetDragDismissTest {

    @get:Rule
    val compose = createAndroidComposeRule<ComponentActivity>()

    /** One film carrying 30 cinemas, so expanding "Kina" makes the sheet list
     *  far longer than the screen — a reliably scrollable Filtry list. */
    private fun longFilms(): List<Film> {
        val cinemas = (1..30).map { i ->
            CinemaShowings(cinema = "Kino %02d".format(i), cinemaURL = null,
                showtimes = listOf(Showtime("18:00")))
        }
        return listOf(
            Film(title = "Film", posterURL = null,
                showings = listOf(DayShowings("2026-06-14", "label", cinemas)))
        )
    }

    private fun seedVm(): KinowoViewModel {
        val ctx = ApplicationProvider.getApplicationContext<Context>()
        val http = OkHttpClient()
        val repo = RepertoireRepository(KinowoApi(client = http), JsonListCache(ctx.cacheDir, "rep_filt", Film.serializer()))
        val detailsRepo = DetailsRepository(KinowoApi(client = http), JsonListCache(ctx.cacheDir, "det_filt", FilmDetails.serializer()))
        val authRepo = AuthRepository(http, PersistentCookieJar(ctx))
        val noop = object : UserStateClient {
            override suspend fun fetchState() = UserSyncState(emptySet(), emptySet())
            override suspend fun putState(state: UserSyncState) {}
        }
        return KinowoViewModel(repo, detailsRepo, UserPreferences(ctx), authRepo, noop)
    }

    @OptIn(ExperimentalMaterial3Api::class)
    private fun mount(onDismiss: () -> Unit) {
        val vm = seedVm()
        val films = longFilms()
        compose.setContent {
            KinowoTheme {
                val sheetState = rememberModalBottomSheetState(skipPartiallyExpanded = true)
                FiltersSheet(vm, films, sheetState = sheetState, onDismiss = onDismiss)
            }
        }
        compose.waitForIdle()
        // Expand "Kina" so the list overflows the screen and is clearly scrollable.
        compose.onNodeWithText("Kina").performClick()
        compose.waitForIdle()
    }

    /** A strong downward swipe over most of the sheet, started near the top of the
     *  list — it rolls the list to its top almost at once and the large remainder
     *  (drag + fling) spills past it, which is what the unfixed sheet uses to
     *  drag-to-close. */
    private fun swipeDownHard() {
        compose.onNode(hasScrollAction()).performTouchInput {
            val x = width / 2f
            down(Offset(x, height * 0.12f))
            moveTo(Offset(x, height * 0.5f))
            moveTo(Offset(x, height * 0.95f))
            up()
        }
        compose.waitForIdle()
    }

    @Test
    fun dragStartedBelowTheTopScrollsUpAndDoesNotCloseTheSheet() {
        var dismissed = false
        mount(onDismiss = { dismissed = true })

        // Put the list JUST below its top (not at the top): scroll down to the last
        // cinema, then back up to the first — leaving only the few header rows
        // (Filtry / Sortuj / Kina) above it. A strong downward swipe then rolls the
        // list to its top almost immediately, so nearly the whole gesture spills
        // past the top — exactly what the unfixed sheet turns into drag-to-close.
        compose.onNode(hasScrollAction()).performScrollToNode(hasText("Kino 30"))
        compose.waitForIdle()
        compose.onNode(hasScrollAction()).performScrollToNode(hasText("Kino 01"))
        compose.waitForIdle()
        swipeDownHard()

        assertFalse("A downward drag that started below the top must not close the Filtry sheet", dismissed)
    }
}

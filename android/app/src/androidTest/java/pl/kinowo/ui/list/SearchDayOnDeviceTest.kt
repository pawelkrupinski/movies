package pl.kinowo.ui.list

import android.content.Context
import androidx.compose.ui.test.assertIsSelected
import androidx.compose.ui.test.hasClickAction
import androidx.compose.ui.test.hasSetTextAction
import androidx.compose.ui.test.hasText
import androidx.compose.ui.test.performClick
import androidx.compose.ui.test.performTextClearance
import androidx.compose.ui.test.performTextInput
import androidx.test.core.app.ApplicationProvider
import androidx.test.ext.junit.runners.AndroidJUnit4
import java.time.LocalDate
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import kotlinx.coroutines.runBlocking
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import pl.kinowo.R
import pl.kinowo.RecreationProofComposeRule
import pl.kinowo.StaticRepertoireApi
import pl.kinowo.data.JsonListCache
import pl.kinowo.data.RepertoireRepository
import pl.kinowo.model.CinemaShowings
import pl.kinowo.model.DayShowings
import pl.kinowo.model.Film
import pl.kinowo.model.Showtime
import pl.kinowo.testKinowoViewModel
import pl.kinowo.ui.theme.KinowoTheme

/**
 * On the REAL [ListScreen]: a search looks through every day. Typing an original title moves
 * the pills to "All" and finds a film showing only tomorrow; clearing it goes back to today --
 * unless a day was picked while it ran, which stays.
 */
@RunWith(AndroidJUnit4::class)
class SearchDayOnDeviceTest {

    @get:Rule
    val compose = RecreationProofComposeRule()

    private val context: Context get() = ApplicationProvider.getApplicationContext()

    private fun mountSeeded() {
        val zone = ZoneId.of("Europe/Warsaw")
        val today = LocalDate.now(zone).format(DateTimeFormatter.ISO_DATE)
        val tomorrow = LocalDate.now(zone).plusDays(1).format(DateTimeFormatter.ISO_DATE)
        fun on(date: String) = DayShowings(date, "label", listOf(CinemaShowings("Muza", showtimes = listOf(Showtime("23:50")))))
        val films = listOf(
            Film(title = "Dziś grany", showings = listOf(on(today))),
            Film(title = "Milczenie owiec", searchTitles = listOf("The Silence of the Lambs"), showings = listOf(on(tomorrow))),
        )
        val repository = RepertoireRepository(StaticRepertoireApi(films), JsonListCache(context.cacheDir, "rep_search_day", Film.serializer()))
        runBlocking { repository.reload("warszawa") }
        val viewModel = testKinowoViewModel(context, repository)
        compose.setContent { KinowoTheme { ListScreen(viewModel, onOpenFilm = {}) } }
        compose.waitUntil(SettleTimeoutMs) { cardShown("Dziś grany") }
    }

    private fun cardShown(title: String): Boolean =
        compose.onAllNodes(hasText(title)).fetchSemanticsNodes().isNotEmpty()

    private fun searchField() = compose.onNode(hasSetTextAction())

    /** The day pill, not the day name the screen flashes after a day change (also its label). */
    private fun pill(label: Int) = compose.onNode(hasText(context.getString(label)).and(hasClickAction()))

    @Test
    fun aSearchByOriginalTitleShowsEveryDayAndClearingGoesBack() {
        mountSeeded()
        searchField().performTextInput("silence of the lambs")
        compose.waitUntil(SettleTimeoutMs) { cardShown("Milczenie owiec") }
        pill(R.string.all).assertIsSelected()

        searchField().performTextClearance()
        compose.waitUntil(SettleTimeoutMs) { cardShown("Dziś grany") }
        pill(R.string.date_today).assertIsSelected()
    }

    @Test
    fun aDayPickedDuringTheSearchStaysWhenItIsCleared() {
        mountSeeded()
        searchField().performTextInput("silence")
        compose.waitUntil(SettleTimeoutMs) { cardShown("Milczenie owiec") }
        pill(R.string.date_tomorrow).performClick()
        compose.waitForIdle()
        searchField().performTextClearance()
        compose.waitForIdle()
        pill(R.string.date_tomorrow).assertIsSelected()
    }

    private companion object {
        const val SettleTimeoutMs = 10_000L
    }
}

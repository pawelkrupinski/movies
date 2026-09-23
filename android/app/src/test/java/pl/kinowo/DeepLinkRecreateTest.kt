package pl.kinowo

import android.app.Application
import android.content.Intent
import android.net.Uri
import androidx.compose.ui.test.junit4.createEmptyComposeRule
import androidx.compose.ui.test.onAllNodesWithContentDescription
import androidx.compose.ui.test.onNodeWithContentDescription
import androidx.compose.ui.test.performClick
import androidx.lifecycle.ViewModel
import androidx.lifecycle.ViewModelProvider
import androidx.test.core.app.ActivityScenario
import androidx.test.core.app.ApplicationProvider
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertEquals
import org.junit.Rule
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import org.robolectric.annotation.GraphicsMode
import pl.kinowo.data.FreshUserPreferences
import pl.kinowo.data.JsonListCache
import pl.kinowo.data.RepertoireRepository
import pl.kinowo.model.CinemaShowings
import pl.kinowo.model.Country
import pl.kinowo.model.DayShowings
import pl.kinowo.model.Film
import pl.kinowo.model.Showtime
import pl.kinowo.net.KinowoApi
import pl.kinowo.net.RepertoireApi
import pl.kinowo.ui.KinowoViewModel
import java.time.LocalDate
import java.time.ZoneId

/**
 * A nav deep link is handled ONCE. `recreate()` — a rotation, a language
 * switch — hands the relaunched MainActivity the last `setIntent` intent, so a
 * link handled from `onCreate`/`onNewIntent` used to replay: its filters
 * overwrote whatever the user had changed since, and the film page reopened.
 * The one recreate that SHOULD carry a link forward — a link into another
 * country, whose switch clears the ViewModel — does so through a pending link
 * persisted with the country + city, applied by the fresh ViewModel.
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34], application = OfflineKinowoApplication::class)
@GraphicsMode(GraphicsMode.Mode.NATIVE)
class DeepLinkRecreateTest {

    @get:Rule(order = 0)
    val fresh = FreshUserPreferences()

    @get:Rule(order = 1)
    val compose = createEmptyComposeRule()

    private val back get() = ApplicationProvider.getApplicationContext<Application>().getString(R.string.back)

    private fun launch(url: String): ActivityScenario<MainActivity> = ActivityScenario.launch(
        Intent(Intent.ACTION_VIEW, Uri.parse(url), ApplicationProvider.getApplicationContext(), MainActivity::class.java),
    )

    private fun detailShown() = compose.onAllNodesWithContentDescription(back).fetchSemanticsNodes().isNotEmpty()

    private fun viewModel(scenario: ActivityScenario<MainActivity>): KinowoViewModel {
        lateinit var vm: KinowoViewModel
        scenario.onActivity { vm = ViewModelProvider(it)[KinowoViewModel::class.java] }
        return vm
    }

    /** Idle for a while: long enough for a replayed link to have re-applied. */
    private fun settle() {
        val end = System.currentTimeMillis() + 1_000
        while (System.currentTimeMillis() < end) { compose.waitForIdle(); Thread.sleep(20) }
    }

    @Test
    fun recreateAfterALinkDoesNotReapplyItsFiltersOrReopenTheFilm() {
        fresh.write { setCityInCountry("warszawa", "pl") }
        launch("https://kinowo.net/warszawa/movie?title=${OfflineKinowoApplication.TARGET}&q=duna").use { scenario ->
            compose.waitUntil(10_000) { detailShown() }
            // The user moves on: clears the link's search, backs out of the film.
            viewModel(scenario).search = ""
            compose.onNodeWithContentDescription(back).performClick()
            compose.waitUntil(5_000) { !detailShown() }

            scenario.recreate()
            settle()

            assertEquals("the link's search must not come back", "", viewModel(scenario).search)
            assertEquals("the film page must not reopen", false, detailShown())
        }
    }

    @Test
    fun aCrossCountryLinkStillOpensTheFilmAfterTheCountrySwitch() {
        fresh.write { setCityInCountry("warszawa", "pl") }
        launch("https://showtimes.cc/uk/london/movie?title=${OfflineKinowoApplication.TARGET}").use {
            compose.waitUntil(10_000) { detailShown() }

            assertEquals("uk" to "london", runBlocking { fresh.prefs.countryAndCity.first() })
        }
    }
}

package pl.kinowo.data

import androidx.test.core.app.ApplicationProvider
import kotlinx.coroutines.flow.first
import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertEquals
import org.junit.After
import org.junit.Before
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config

/**
 * hiddenFilms is per country — server-side (`/api/me/{country}/hidden-films`)
 * and so locally too. A single device-wide set let one country's hides leak
 * into another on a country switch: the next reconcile unioned them into the
 * new country and uploaded them, or (on a 304) simply kept showing them.
 *
 * The DataStore is process-wide under Robolectric, so every test starts and
 * ends on an empty store — which also means "never picked a country" (Poland).
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class UserPreferencesHiddenFilmsPerCountryTest {

    private val prefs = UserPreferences(ApplicationProvider.getApplicationContext())

    // Other classes in the fork assert "null until set" on the country and
    // city, so leave the store as empty as it was found.
    @Before
    @After
    fun reset() {
        runBlocking { prefs.clearAllForTest() }
    }

    @Test
    fun aCountrySwitchShowsThatCountrysOwnSetAndSwitchingBackRestoresTheFirst() = runBlocking {
        prefs.hide("Film PL")

        prefs.setCountryCode("uk")
        assertEquals(emptySet<String>(), prefs.hiddenFilms.first())
        prefs.hide("Film UK")

        prefs.setCityInCountry("warszawa", "pl")
        assertEquals(setOf("Film PL"), prefs.hiddenFilms.first())
        assertEquals(setOf("Film UK"), prefs.hiddenFilmsFor("uk"))
    }

    @Test
    fun unhideAllClearsOnlyTheCurrentCountry() = runBlocking {
        prefs.setHiddenFilms("uk", setOf("Film UK"))
        prefs.hide("Film PL")

        prefs.unhideAll()

        assertEquals(emptySet<String>(), prefs.hiddenFilms.first())
        assertEquals(setOf("Film UK"), prefs.hiddenFilmsFor("uk"))
    }

    @Test
    fun aNeverPickedCountryIsTheDefaultOne() = runBlocking {
        prefs.hide("Film PL")
        assertEquals(setOf("Film PL"), prefs.hiddenFilmsFor("pl"))
    }

    /** An upgrade from the device-wide set: it belongs to the country the
     *  device is browsing, and stays with THAT country after a switch rather
     *  than following the user into the next one. */
    @Test
    fun theLegacyDeviceWideSetStaysWithTheCountryItWasMadeIn() = runBlocking {
        prefs.writeLegacyHiddenFilms(setOf("Legacy"))
        assertEquals(setOf("Legacy"), prefs.hiddenFilms.first())

        prefs.setCountryCode("de")
        assertEquals(emptySet<String>(), prefs.hiddenFilms.first())

        prefs.setCountryCode("pl")
        assertEquals(setOf("Legacy"), prefs.hiddenFilms.first())
    }

    @Test
    fun aHideOnTopOfTheLegacySetKeepsIt() = runBlocking {
        prefs.writeLegacyHiddenFilms(setOf("Legacy"))

        prefs.hide("New")

        assertEquals(setOf("Legacy", "New"), prefs.hiddenFilms.first())
    }
}

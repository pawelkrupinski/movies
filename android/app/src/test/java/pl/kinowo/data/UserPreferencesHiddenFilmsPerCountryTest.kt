package pl.kinowo.data

import kotlinx.coroutines.flow.first
import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Rule
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
 * [FreshUserPreferences] starts every test on an empty store — which also
 * means "never picked a country" (Poland).
 */
@RunWith(RobolectricTestRunner::class)
@Config(sdk = [34])
class UserPreferencesHiddenFilmsPerCountryTest {

    @get:Rule
    val fresh = FreshUserPreferences()

    private val prefs get() = fresh.prefs

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

    /** The device-wide set was reconciled against whichever country was
     *  selected, so the per-country validators an older build stored describe
     *  server sets this device never kept apart: replaying them would draw a
     *  304 and strand the upgraded set (possibly another country's titles, or
     *  an empty never-written bucket) under a "you're current" answer. None is
     *  trusted while the legacy set is unsettled, and settling it forgets them
     *  all — the migrated flags stay, so each country's next reconcile takes a
     *  fresh 200 and REPLACES its bucket. Mirrors iOS. */
    @Test
    fun theUpgradeForgetsTheValidatorsTheDeviceWideSetWasSyncedUnder() = runBlocking {
        prefs.setHiddenFilmsMigrated("pl", true)
        prefs.setHiddenFilmsMigrated("uk", true)
        prefs.setHiddenFilmsValidators("pl", "\"pl\"", "lm-pl")
        prefs.setHiddenFilmsValidators("uk", "\"uk\"", "lm-uk")
        prefs.writeLegacyHiddenFilms(setOf("Legacy"))

        assertNull(prefs.hiddenFilmsEtag("pl"))
        assertNull(prefs.hiddenFilmsLastModified("uk"))

        prefs.setCountryCode("uk")

        assertNull(prefs.hiddenFilmsEtag("uk"))
        assertNull(prefs.hiddenFilmsEtag("pl"))
        assertTrue(prefs.isHiddenFilmsMigrated("uk"))
    }

    /** A hide made before the first reconcile folds the legacy set too — and
     *  forgets the validators the same way. */
    @Test
    fun aHideThatSettlesTheLegacySetForgetsTheValidatorsToo() = runBlocking {
        prefs.setHiddenFilmsValidators("uk", "\"uk\"", "lm-uk")
        prefs.writeLegacyHiddenFilms(setOf("Legacy"))

        prefs.hide("New")

        assertNull(prefs.hiddenFilmsEtag("uk"))
    }

    /** The queue of unsent writes keeps its order and any title verbatim
     *  (colons included), per country, and a logout forgets it. */
    @Test
    fun pendingWritesRoundTripInOrderAndALogoutForgetsThem() = runBlocking {
        val ops = listOf(HiddenFilmsOp.Hide("Mission: Impossible"), HiddenFilmsOp.Clear, HiddenFilmsOp.Unhide("B"))
        prefs.setPendingHiddenFilmsOps("pl", ops)

        assertEquals(ops, prefs.pendingHiddenFilmsOps("pl"))
        assertEquals(emptyList<HiddenFilmsOp>(), prefs.pendingHiddenFilmsOps("uk"))

        prefs.clearHiddenFilmsSyncState()
        assertEquals(emptyList<HiddenFilmsOp>(), prefs.pendingHiddenFilmsOps("pl"))
    }
}

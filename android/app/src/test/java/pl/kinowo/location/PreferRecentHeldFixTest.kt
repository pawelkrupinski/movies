package pl.kinowo.location

import kotlinx.coroutines.test.runTest
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Test

/**
 * Which fix the nearer-city check and the gate use. Android used to take the
 * held `lastLocation` whatever its age, while iOS asks for a fresh fix once the
 * held one is over 15 minutes old — so the same phone could be offered a city
 * from hours ago on one platform and today's on the other.
 */
class PreferRecentHeldFixTest {

    private data class Fix(val city: String, val ageMs: Long)

    private val minutes = 60_000L

    @Test
    fun aRecentHeldFixAnswersWithoutAskingForAFreshOne() = runTest {
        var asked = false
        val fix = preferRecentHeldFix(Fix("poznan", 5 * minutes), Fix::ageMs) { asked = true; Fix("warszawa", 0) }
        assertEquals("poznan", fix?.city)
        assertEquals(false, asked)
    }

    @Test
    fun anOldHeldFixAsksForAFreshOneThatWins() = runTest {
        val fix = preferRecentHeldFix(Fix("poznan", 60 * minutes), Fix::ageMs) { Fix("warszawa", 0) }
        assertEquals("warszawa", fix?.city)
    }

    @Test
    fun anOldHeldFixStillAnswersWhenNoFreshOneComes() = runTest {
        val fix = preferRecentHeldFix(Fix("poznan", 60 * minutes), Fix::ageMs) { null }
        assertEquals("poznan", fix?.city)
    }

    @Test
    fun noHeldFixAndNoFreshOneIsNoFix() = runTest {
        assertNull(preferRecentHeldFix<Fix>(null, Fix::ageMs) { null })
    }
}

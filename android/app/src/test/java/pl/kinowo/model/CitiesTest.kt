package pl.kinowo.model

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Test

/**
 * Pins the location → city mapping used by the first-launch gate: near a
 * supported city we resolve to it; far from every supported city we resolve to
 * null so the gate falls back to an explicit pick.
 */
class CitiesTest {

    @Test
    fun resolvesPoznanFromItsOwnCoordinates() {
        val city = Cities.nearestWithin100km(52.4064, 16.9252)
        assertEquals("poznan", city?.slug)
    }

    @Test
    fun resolvesPoznanFromNearby() {
        // ~30 km outside Poznań is still within range.
        val city = Cities.nearestWithin100km(52.40, 16.50)
        assertEquals("poznan", city?.slug)
    }

    @Test
    fun resolvesEachSupportedCityFromItsOwnCoordinates() {
        assertEquals("wroclaw", Cities.nearestWithin100km(51.1079, 17.0385)?.slug)
        assertEquals("warszawa", Cities.nearestWithin100km(52.2297, 21.0122)?.slug)
    }

    @Test
    fun returnsNullWhenFartherThan100km() {
        // Kraków (50.06, 19.94) is >200 km from Poznań, Wrocław and Warszawa —
        // out of range of every supported city.
        assertNull(Cities.nearestWithin100km(50.0647, 19.9450))
    }
}

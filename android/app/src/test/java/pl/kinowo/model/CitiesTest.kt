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

    @Test
    fun suggestsTheNearerCityWhenChosenIsElsewhere() {
        // Chosen Poznań, but standing in Wrocław → offer the switch.
        val suggestion = Cities.switchSuggestion("poznan", 51.1079, 17.0385, lastPromptKey = null)
        assertEquals("wroclaw", suggestion?.target?.slug)
        assertEquals("poznan→wroclaw", suggestion?.key)
    }

    @Test
    fun doesNotRepeatTheSamePair() {
        // Already asked poznan→wroclaw — don't ask again for the same pair.
        val suggestion = Cities.switchSuggestion(
            "poznan", 51.1079, 17.0385, lastPromptKey = "poznan→wroclaw",
        )
        assertNull(suggestion)
    }

    @Test
    fun noSuggestionWhenAlreadyInTheNearestCity() {
        // Chosen Wrocław, standing in Wrocław → nothing to switch to.
        assertNull(Cities.switchSuggestion("wroclaw", 51.1079, 17.0385, lastPromptKey = null))
    }

    @Test
    fun noSuggestionWhenOutOfRangeOfEveryCity() {
        // Kraków is out of range of every supported city → no offer.
        assertNull(Cities.switchSuggestion("poznan", 50.0647, 19.9450, lastPromptKey = null))
    }
}

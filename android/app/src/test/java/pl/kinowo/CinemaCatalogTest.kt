package pl.kinowo

import kotlinx.serialization.json.Json
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import pl.kinowo.model.CinemaCatalog
import pl.kinowo.model.CinemaSeason

/**
 * Decoding of `GET /:city/api/cinemas` into [CinemaCatalog] — the payload the
 * split-city area picker is built from. A split city carries areas that
 * partition its cinema list; a flat city carries an empty `areas`.
 */
class CinemaCatalogTest {

    private val json = Json { ignoreUnknownKeys = true }

    @Test
    fun decodesSplitCityAreas() {
        val body = """
            {"cinemas":["A Cinema","B Cinema","C Cinema"],
             "areas":[{"name":"Central","slug":"central","cinemas":["A Cinema"]},
                      {"name":"North","slug":"north","cinemas":["B Cinema","C Cinema"]}]}
        """.trimIndent()

        val catalog = json.decodeFromString<CinemaCatalog>(body)

        assertTrue(catalog.isSplit)
        assertEquals(3, catalog.cinemas.size)
        assertEquals(listOf("Central", "North"), catalog.areas.map { it.name })
        assertEquals(listOf("central", "north"), catalog.areas.map { it.slug })
        assertEquals(listOf("B Cinema", "C Cinema"), catalog.areas[1].cinemas)
        // The areas partition the cinema universe.
        assertEquals(catalog.cinemas.toSet(), catalog.areas.flatMap { it.cinemas }.toSet())
    }

    @Test
    fun decodesFlatCityWithEmptyAreas() {
        val body = """{"cinemas":["Only Cinema"],"areas":[]}"""

        val catalog = json.decodeFromString<CinemaCatalog>(body)

        assertFalse(catalog.isSplit)
        assertEquals(listOf("Only Cinema"), catalog.cinemas)
        assertTrue(catalog.areas.isEmpty())
    }

    @Test
    fun emptyCatalogIsFlat() {
        assertFalse(CinemaCatalog.EMPTY.isSplit)
        assertTrue(CinemaCatalog.EMPTY.cinemas.isEmpty())
    }

    @Test
    fun cinemasToDisableExcludesUncheckedAreas() {
        val catalog = json.decodeFromString<CinemaCatalog>(
            """{"cinemas":["A Cinema","B Cinema","C Cinema"],
                "areas":[{"name":"Central","slug":"central","cinemas":["A Cinema"]},
                         {"name":"North","slug":"north","cinemas":["B Cinema","C Cinema"]}]}"""
        )
        // Keep only Central → North's cinemas are disabled.
        assertEquals(listOf("B Cinema", "C Cinema"), catalog.cinemasToDisable(setOf("central")))
        // Keep all → nothing disabled (the pre-selected default is a no-op).
        assertTrue(catalog.cinemasToDisable(setOf("central", "north")).isEmpty())
        // Keep none → everything disabled.
        assertEquals(catalog.cinemas.toSet(), catalog.cinemasToDisable(emptySet()).toSet())
    }

    private val berlin = """
        {"cinemas":["Freiluftkino Kreuzberg","Kino Central Berlin","Freiluftkino Rehberge"],"areas":[],
         "seasons":[{"cinema":"Freiluftkino Kreuzberg","opens":5,"closes":9,"offSeason":true},
                    {"cinema":"Freiluftkino Rehberge","opens":5,"closes":9,"offSeason":false}]}
    """.trimIndent()

    /** The server decides which venues are out of season; the filter sinks them to the bottom. */
    @Test
    fun outOfSeasonCinemasSinkToTheBottomOnTheServersVerdict() {
        val catalog = json.decodeFromString<CinemaCatalog>(berlin)

        assertTrue(catalog.isOffSeason("Freiluftkino Kreuzberg"))
        assertFalse(catalog.isOffSeason("Freiluftkino Rehberge"))   // seasonal, in season
        assertFalse(catalog.isOffSeason("Kino Central Berlin"))
        assertEquals(
            listOf("Kino Central Berlin", "Freiluftkino Rehberge", "Freiluftkino Kreuzberg"),
            catalog.seasonOrdered(catalog.cinemas),
        )
    }

    /** The muted second line of an out-of-season row, in each UI language. */
    @Test
    fun backInSeasonLineInEachLanguage() {
        val season = json.decodeFromString<CinemaCatalog>(berlin).seasonOf("Freiluftkino Kreuzberg")!!

        assertEquals("back in season · May–Sept", season.backInSeasonLine("en"))
        assertEquals("öffnet wieder · Mai–Sept.", season.backInSeasonLine("de"))
        assertEquals("wraca w sezonie · maj–wrz", season.backInSeasonLine("pl"))
        assertEquals("vuelve en temporada · may–sept", season.backInSeasonLine("es"))
    }

    /** A month outside 1..12 from the server reads as the bare "back in season", never a crash. */
    @Test
    fun backInSeasonLineWithAMonthOutOfRangeDropsTheWindow() {
        for ((opens, closes) in listOf(0 to 9, 5 to 13, -1 to 12, 13 to 0)) {
            val season = CinemaSeason("Kino", opens, closes, offSeason = true)
            assertEquals("$opens–$closes", "back in season", season.backInSeasonLine("en"))
        }
    }

    /** A catalog from a server that predates seasons decodes with none, and nothing sinks. */
    @Test
    fun catalogWithoutSeasonsHasNoneOutOfSeason() {
        val catalog = json.decodeFromString<CinemaCatalog>("""{"cinemas":["A","B"],"areas":[]}""")
        assertTrue(catalog.seasons.isEmpty())
        assertEquals(listOf("A", "B"), catalog.seasonOrdered(catalog.cinemas))
    }
}

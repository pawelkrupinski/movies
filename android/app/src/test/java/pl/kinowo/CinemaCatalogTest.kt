package pl.kinowo

import kotlinx.serialization.json.Json
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import pl.kinowo.model.CinemaCatalog

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
}

package pl.kinowo.data

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

/**
 * The catalog is persisted to durable local storage so a relaunch starts from
 * the last fetch. A fresh [CatalogCache] over the same directory reads back
 * exactly what a prior one saved — the relaunch path.
 */
class CatalogCacheTest {

    @get:Rule
    val tmp = TemporaryFolder()

    @Test
    fun emptyUntilSaved() {
        assertNull(CatalogCache(tmp.root).load())
    }

    @Test
    fun saveThenLoadRoundTripsAcrossInstances() {
        val body = """{"countries":[],"cities":[]}"""
        CatalogCache(tmp.root).save(body, "\"abc123\"")

        val loaded = CatalogCache(tmp.root).load()   // fresh instance = next launch
        assertEquals(body, loaded?.body)
        assertEquals("\"abc123\"", loaded?.etag)
    }

    @Test
    fun saveOverwritesPrevious() {
        CatalogCache(tmp.root).save("old", "\"1\"")
        CatalogCache(tmp.root).save("new", "\"2\"")
        assertEquals("new", CatalogCache(tmp.root).load()?.body)
        assertEquals("\"2\"", CatalogCache(tmp.root).load()?.etag)
    }
}

package pl.kinowo.data

import kotlinx.coroutines.runBlocking
import org.junit.Assert.assertEquals
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder
import pl.kinowo.net.CatalogApi
import pl.kinowo.net.KinowoApi

/**
 * The repository seeds from the bundle, refreshes with a conditional GET, and
 * PERSISTS a 200 so a relaunch (a new repository over the same cache) starts
 * from the last fetch — not the seed.
 */
class CatalogRepositoryTest {

    @get:Rule
    val tmp = TemporaryFolder()

    private val ukBody =
        """{"countries":[{"code":"uk","name":"United Kingdom","baseUrl":"https://showtimes-uk.fly.dev","language":"en","brand":"Showtimes"}],"cities":[{"slug":"london","name":"London","lat":51.5,"lon":-0.1,"country":"uk"}]}"""
    private val plSeed =
        """{"etag":"\"seed\"","catalog":{"countries":[{"code":"pl","name":"Polska","baseUrl":"https://kinowo.fly.dev","language":"pl","brand":"Kinowo"}],"cities":[{"slug":"poznan","name":"Poznań","lat":52.4,"lon":16.9,"country":"pl"}]}}"""

    private fun api(result: KinowoApi.FetchedCatalog) = CatalogApi { result }

    @Test
    fun seedsFromBundleUntilFetch() {
        val repo = CatalogRepository(api(KinowoApi.FetchedCatalog(null, null, notModified = true)), CatalogCache(tmp.root), plSeed)
        assertEquals("poznan", repo.catalog.value.cities.first().slug)
    }

    @Test
    fun reloadPersistsAndSurvivesRelaunch() = runBlocking {
        val cache = CatalogCache(tmp.root)
        val repo = CatalogRepository(api(KinowoApi.FetchedCatalog(ukBody, "\"uk\"", notModified = false)), cache, plSeed)
        repo.reload()
        assertEquals("london", repo.catalog.value.cities.first().slug)   // fetched replaces the seed

        // A NEW repository over the SAME cache loads the PERSISTED catalog (uk),
        // NOT the seed (pl) — proof it's persisted to local storage.
        val relaunched = CatalogRepository(api(KinowoApi.FetchedCatalog(null, null, notModified = true)), cache, plSeed)
        assertEquals("london", relaunched.catalog.value.cities.first().slug)
    }

    @Test
    fun notModifiedKeepsCurrentCatalog() = runBlocking {
        val repo = CatalogRepository(api(KinowoApi.FetchedCatalog(null, null, notModified = true)), CatalogCache(tmp.root), plSeed)
        repo.reload()
        assertEquals("poznan", repo.catalog.value.cities.first().slug)   // 304 → seed stands
    }
}

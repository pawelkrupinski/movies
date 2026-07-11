package pl.kinowo

import org.junit.Assert.assertTrue
import org.junit.Assert.assertEquals
import org.junit.Test
import java.io.File

/**
 * Guards the Google Play store listing text (`src/main/play/listings/<locale>/`)
 * that Gradle Play Publisher uploads. There's no runtime/emulator surface for
 * store metadata, so this is the only automated check: it pins that every
 * locale's title / short / full description stays within Play's hard character
 * limits (an over-limit string fails `publishReleaseListing` at upload time,
 * after review has already been triggered), and that the UK (`en-GB`) listing
 * exists and leads with the English brand "Showtimes" (Poland keeps "Kinowo").
 */
class PlayListingTest {

    // Google Play field limits.
    private val titleMax = 30
    private val shortMax = 80
    private val fullMax = 4000

    private val listingsDir: File = run {
        // Gradle runs unit tests with user.dir at the module dir; tolerate the
        // repo-root / android-root cases too so the test isn't cwd-fragile.
        val base = File(System.getProperty("user.dir") ?: ".")
        listOf("src/main/play/listings", "app/src/main/play/listings", "android/app/src/main/play/listings")
            .map { base.resolve(it) }
            .firstOrNull { it.isDirectory }
            ?: error("Play listings dir not found from $base")
    }

    private fun trimmed(locale: String, name: String): String =
        listingsDir.resolve("$locale/$name").readText().trim()

    @Test
    fun everyLocaleStaysWithinPlayLimits() {
        val locales = listingsDir.listFiles { f -> f.isDirectory }!!.map { it.name }
        assertTrue("expected pl-PL and en-GB listings, found $locales",
            locales.containsAll(listOf("pl-PL", "en-GB")))
        for (locale in locales) {
            assertTrue("$locale/title.txt over $titleMax", trimmed(locale, "title.txt").length <= titleMax)
            assertTrue("$locale/short-description.txt over $shortMax", trimmed(locale, "short-description.txt").length <= shortMax)
            assertTrue("$locale/full-description.txt over $fullMax", trimmed(locale, "full-description.txt").length <= fullMax)
        }
    }

    @Test
    fun ukListingLeadsWithTheEnglishBrand() {
        // Outside Poland the app is branded "Showtimes"; the store title must lead with it.
        assertTrue(trimmed("en-GB", "title.txt").startsWith("Showtimes"))
        assertTrue("full description should not be empty", trimmed("en-GB", "full-description.txt").isNotEmpty())
    }

    @Test
    fun polishListingKeepsTheKinowoBrand() {
        assertTrue(trimmed("pl-PL", "title.txt").startsWith("Kinowo"))
    }
}

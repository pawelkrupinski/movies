package pl.kinowo

import org.junit.Assert.assertFalse
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

    /** `src/main/play/` — the listings' parent, where the contact fields live. */
    private val playDir: File = listingsDir.parentFile

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

    /**
     * Play has no keywords field — the short description IS the ranking surface,
     * so leaving most of its 80 characters unused throws away the app's best
     * search signal. Polish sat at 29/80 ("Repertuar kin w Twoim mieście") while
     * English and German spent ~70, and nothing flagged it: an under-length
     * string is perfectly valid to Play.
     */
    @Test
    fun everyShortDescriptionSpendsItsKeywordBudget() {
        val floor = 60
        for (locale in listingsDir.listFiles { f -> f.isDirectory }!!.map { it.name }) {
            val short = trimmed(locale, "short-description.txt")
            assertTrue(
                "$locale/short-description.txt uses only ${short.length} of $shortMax chars " +
                    "(floor $floor) — Play ranks on this text, so spend it: \"$short\"",
                short.length >= floor)
        }
    }

    @Test
    fun polishListingKeepsTheKinowoBrand() {
        assertTrue(trimmed("pl-PL", "title.txt").startsWith("Kinowo"))
    }

    /**
     * Play's `contact-website.txt` is the SUPPORT WEBSITE field — the one a
     * reviewer and a user click for help, not a second link to the product.
     *
     * It drifted twice: the live value was still `kinowo.fly.dev`, retired when
     * the site moved to its own domain, and the checked-in value was the bare
     * listings homepage. Both are wrong for a support field, and nothing failed
     * — Play accepts any URL that resolves.
     */
    @Test
    fun contactWebsiteIsTheSupportPageOnALiveDomain() {
        val url = playDir.resolve("contact-website.txt").readText().trim()
        assertFalse("contact website still points at the retired host: $url",
            url.contains("fly.dev"))
        assertTrue("Play's contact website is the support field — point it at the support page, not the listings: $url",
            url.endsWith("/support"))
        assertTrue("support URL must be absolute https: $url", url.startsWith("https://"))
    }
}

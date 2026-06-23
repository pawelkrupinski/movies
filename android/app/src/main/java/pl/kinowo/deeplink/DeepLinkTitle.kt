package pl.kinowo.deeplink

/**
 * Title matching for deep links, mirroring the server's
 * `TitleNormalizer.normalize` (common/…/TitleNormalizer.scala). The web's film
 * page (`MovieController.film`) matches the URL's `?title=` against schedules
 * by `normalize`, NOT byte-for-byte — it folds standalone Arabic numerals to
 * Roman ("…Prady 2" ⇄ "…Prady II") because the linked/displayed title and the
 * stored film title diverge for numbered films. The apps must match the same
 * way, or a deep link to a sequel opens the app but never the film page.
 *
 * Keep in sync with `TitleNormalizer.normalize` / its `ArabicToRoman` map.
 */
object DeepLinkTitle {
    private val arabicToRoman = mapOf(
        "1" to "I", "2" to "II", "3" to "III", "4" to "IV", "5" to "V",
        "6" to "VI", "7" to "VII", "8" to "VIII", "9" to "IX", "10" to "X",
        "11" to "XI", "12" to "XII", "13" to "XIII", "14" to "XIV", "15" to "XV",
        "16" to "XVI", "17" to "XVII", "18" to "XVIII", "19" to "XIX", "20" to "XX",
    )

    /** Standalone Arabic numerals → Roman, word by word. */
    fun normalize(title: String): String =
        title.split(" ").joinToString(" ") { arabicToRoman[it] ?: it }

    /** Whether two titles refer to the same film under the server's normalize. */
    fun matches(a: String, b: String): Boolean = normalize(a) == normalize(b)
}

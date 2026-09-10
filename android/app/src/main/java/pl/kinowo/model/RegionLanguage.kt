package pl.kinowo.model

import android.content.Context
import java.util.Locale

/**
 * Device-region → language table used to pick a sensible default UI language
 * for a user who hasn't made an explicit pick yet. Distinct from [Country],
 * which only covers the app's five DEPLOYED countries (and ties a language to
 * a whole deployment) — this covers any device region the user's phone might
 * report, so a Spanish-market device (say, a Mexican or Argentine phone) still
 * defaults into `values-es` even though there is no `mx`/`ar` deployment.
 *
 * Mirrors the iOS `StorefrontLanguage` table one-for-one (same country-code
 * groupings), so both apps default a given region to the same language.
 */
object RegionLanguage {
    private val german = setOf("DE", "AT", "CH", "LI")
    private val spanish = setOf(
        "ES", "MX", "AR", "CO", "PE", "CL", "VE", "EC", "GT",
        "CU", "BO", "DO", "HN", "PY", "SV", "NI", "CR", "PA", "UY", "PR",
    )

    /** ISO language for a device region [code] (an ISO 3166-1 alpha-2 country
     *  code, uppercase — [java.util.Locale.getCountry]'s format), or "en" for
     *  an unmapped region or a null code (no region reported). */
    fun forCountryCode(code: String?): String = when {
        code == null -> "en"
        code == "PL" -> "pl"
        german.contains(code) -> "de"
        spanish.contains(code) -> "es"
        else -> "en"
    }
}

/**
 * Resolves the UI language to force when the user has NOT made an explicit
 * pick (see [pl.kinowo.data.UserPreferences.selectedLanguageTag]) — the
 * device's own preferred language when the app localizes it, else the
 * language [RegionLanguage] associates with the device's region. A French
 * device (no `values-fr`) falls back via its region rather than defaulting
 * straight to English, the same way the old country-derived behaviour let a
 * German phone browsing the UK deployment read `values-en` deliberately, not
 * by accident.
 */
object LanguageDefault {
    private val supportedLanguages = setOf("pl", "en", "de", "es")

    /** [context]'s own configured locale — NOT the process-wide
     *  [Locale.getDefault] — so this resolves correctly even applied to a
     *  not-yet-attached base [Context] (see [pl.kinowo.MainActivity.attachBaseContext],
     *  which is exactly why this parameter exists). */
    fun resolve(context: Context): String = resolve(context.resources.configuration.locales[0])

    /** Same resolution, off a [Locale] directly — for callers with no
     *  convenient [Context] (e.g. [pl.kinowo.ui.KinowoViewModel], which
     *  deliberately holds no Context reference). Both overloads share this one
     *  rule so the default can never drift between the two call sites. */
    fun resolve(locale: Locale): String =
        if (locale.language in supportedLanguages) locale.language
        else RegionLanguage.forCountryCode(locale.country.ifEmpty { null })
}

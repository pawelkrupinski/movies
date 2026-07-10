package pl.kinowo.ui

import android.content.Context
import android.content.ContextWrapper
import android.content.res.Configuration
import java.util.Locale

/**
 * Forces a specific UI language on a [Context] regardless of the device locale,
 * by overriding the [Configuration] locale on a wrapped context. This is the
 * pre-AppCompat way to pin the app's language (the app is a plain
 * [android.app.ComponentActivity], so `AppCompatDelegate.setApplicationLocales`
 * isn't available without pulling in appcompat); it works on every supported
 * API level and is deterministic — the selected country carries the tag, so a
 * Polish phone browsing the UK deployment still resolves `values-en`.
 *
 * [MainActivity] applies this in `attachBaseContext`, so the whole resource
 * lookup (Compose `stringResource`, `getString`) resolves against the forced
 * locale from the first frame.
 */
object LocaleWrapper {
    /** A copy of [base] whose resources resolve strings in [languageTag]. */
    fun wrap(base: Context, languageTag: String): Context {
        val locale = Locale.forLanguageTag(languageTag)
        Locale.setDefault(locale)
        val config = Configuration(base.resources.configuration)
        config.setLocale(locale)
        return ContextWrapper(base.createConfigurationContext(config))
    }
}

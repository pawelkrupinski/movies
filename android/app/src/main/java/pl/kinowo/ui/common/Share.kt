package pl.kinowo.ui.common

import android.content.ClipData
import android.content.ClipboardManager
import android.content.Context
import android.content.Intent
import android.os.Build
import android.widget.Toast
import androidx.compose.runtime.compositionLocalOf
import pl.kinowo.R
import java.net.URLEncoder

/**
 * The slug of the city the user is currently browsing (`poznan`, `wroclaw`,
 * `bielsko-biala`, …), provided once at the root of the city-gated UI in
 * `KinowoApp` and read wherever a city-scoped link is built. The default is
 * empty — production always provides a real slug behind the city gate; only a
 * stray render outside that provider (never a real screen) would see it.
 */
val LocalCitySlug = compositionLocalOf { "" }

/**
 * Canonical public URL for a film's page — the Android counterpart of the
 * server's `controllers.FilmHref`. The film page is city-scoped
 * (`/<city>/film/<slug>`); a city-less path has no server route and 404s, so the
 * slug of the city the sharer is browsing is required.
 *
 * `filmSlug` is whatever `/api/repertoire` served for the film, deliberately NOT
 * recomputed here: the server's fold covers Polish and German diacritics, ß, and
 * Cyrillic, and a Kotlin reimplementation would be a second copy of those rules,
 * free to drift into links that 404. When it's absent (an older server), the
 * legacy query form is emitted instead — the server still answers it and 301s it
 * onto the slug. Pure JVM (no Android APIs) so it's unit-testable directly.
 */
fun filmShareUrl(citySlug: String, title: String, filmSlug: String? = null): String =
    if (!filmSlug.isNullOrEmpty()) "https://kinowo.net/$citySlug/film/$filmSlug"
    else "https://kinowo.net/$citySlug/film?title=" +
        URLEncoder.encode(title, "UTF-8").replace("+", "%20")

/** Open the system share sheet for a film's public link. Backs the Share
 *  action in `DetailScreen`'s top bar. */
fun shareFilm(context: Context, citySlug: String, title: String, filmSlug: String? = null) {
    val send = Intent(Intent.ACTION_SEND).apply {
        type = "text/plain"
        putExtra(Intent.EXTRA_SUBJECT, title)
        putExtra(Intent.EXTRA_TEXT, filmShareUrl(citySlug, title, filmSlug))
    }
    context.startActivity(Intent.createChooser(send, null))
}

/** Copy a film's public link to the clipboard. Backs the long-press on a
 *  `FilmCard`. Android 13+ shows its own clipboard confirmation chip, so we
 *  stay quiet there to avoid double feedback; on ≤ 12 there's no system
 *  confirmation, so a short toast fills the gap. */
fun copyFilmLink(context: Context, citySlug: String, title: String, filmSlug: String? = null) {
    val clipboard = context.getSystemService(Context.CLIPBOARD_SERVICE) as ClipboardManager
    clipboard.setPrimaryClip(ClipData.newPlainText(title, filmShareUrl(citySlug, title, filmSlug)))
    if (Build.VERSION.SDK_INT <= Build.VERSION_CODES.S_V2) {
        Toast.makeText(context, context.getString(R.string.link_copied), Toast.LENGTH_SHORT).show()
    }
}

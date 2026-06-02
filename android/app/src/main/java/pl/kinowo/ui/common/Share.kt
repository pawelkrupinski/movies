package pl.kinowo.ui.common

import android.content.ClipData
import android.content.ClipboardManager
import android.content.Context
import android.content.Intent
import android.os.Build
import android.widget.Toast
import java.net.URLEncoder

/**
 * Canonical public URL for a film's `/film?title=…` page — the Android
 * counterpart of the server's `controllers.FilmHref`. Mirrors its RFC 3986
 * encoding (spaces → `%20`, not the form `+`) via the same `URLEncoder` +
 * replace, so a link shared from the app is byte-identical to one copied off
 * the website. Pure JVM (no Android APIs) so it's unit-testable directly.
 */
fun filmShareUrl(title: String): String =
    "https://kinowo.fly.dev/film?title=" + URLEncoder.encode(title, "UTF-8").replace("+", "%20")

/** Open the system share sheet for a film's public link. Backs the Share
 *  action in `DetailScreen`'s top bar. */
fun shareFilm(context: Context, title: String) {
    val send = Intent(Intent.ACTION_SEND).apply {
        type = "text/plain"
        putExtra(Intent.EXTRA_SUBJECT, title)
        putExtra(Intent.EXTRA_TEXT, filmShareUrl(title))
    }
    context.startActivity(Intent.createChooser(send, null))
}

/** Copy a film's public link to the clipboard. Backs the long-press on a
 *  `FilmCard`. Android 13+ shows its own clipboard confirmation chip, so we
 *  stay quiet there to avoid double feedback; on ≤ 12 there's no system
 *  confirmation, so a short toast fills the gap. */
fun copyFilmLink(context: Context, title: String) {
    val clipboard = context.getSystemService(Context.CLIPBOARD_SERVICE) as ClipboardManager
    clipboard.setPrimaryClip(ClipData.newPlainText(title, filmShareUrl(title)))
    if (Build.VERSION.SDK_INT <= Build.VERSION_CODES.S_V2) {
        Toast.makeText(context, "Skopiowano link", Toast.LENGTH_SHORT).show()
    }
}

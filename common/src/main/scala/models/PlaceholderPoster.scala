package models

import java.util.Locale

/**
 * Some cinemas serve a generic "coming soon" graphic instead of a real poster
 * for not-yet-released films. Multikino's CMS does this with
 * `…/film-and-events/wkrotce_1_plakat.jpg` ("wkrótce" = "soon"). Such a URL is
 * a valid image, so we keep it in the record — a film with no other art should
 * still show *something* — but it must rank below every real poster (any other
 * cinema's, and TMDB's) when picking what to display. See
 * `MovieRecord.posterUrl` / `fallbackPosterUrls`.
 */
object PlaceholderPoster {

  /** Marks a poster URL as a known placeholder rather than real cover art.
   *  Case-insensitive; matches the distinctive `wkrotce` segment Multikino's
   *  "coming soon" images carry. */
  def matches(url: String): Boolean =
    url.toLowerCase(Locale.ROOT).contains("wkrotce")
}

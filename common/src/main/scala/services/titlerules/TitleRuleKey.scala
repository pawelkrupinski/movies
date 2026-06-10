package services.titlerules

import models.Cinema

import java.util.Locale

/** Maps a `Cinema` to the per-client rule key its title-cleanup rules live under.
 *
 *  Per the "per client" model: a chain (Cinema City, Helios, Multikino) is one
 *  scraper serving many venue `Cinema`s, so all its venues share a single key.
 *  A standalone cinema keys on a slug of its own display name.
 *
 *  This is the single source of truth for the mapping — used both when scraping
 *  (to clean the freshly-scraped title) and when re-keying stored records during
 *  a backfill (where only the persisted `Cinema`/`Source` is available, never the
 *  scraper), so it MUST be derivable from the `Cinema` alone. */
object TitleRuleKey {

  def of(cinema: Cinema): String = of(cinema.displayName)

  // Multi-venue platform clients whose venues don't share a display-name prefix
  // but DO share one scraper (and thus one cleanup rule set).
  private val bokVenues = Set("Kino na Boku", "Kino Głębocka 66")

  def of(displayName: String): String =
    if (displayName.startsWith("Cinema City")) "cinema-city"
    else if (displayName.startsWith("Helios")) "helios"
    else if (displayName.startsWith("Multikino")) "multikino"
    else if (bokVenues.contains(displayName)) "bok"
    else slug(displayName)

  private def slug(s: String): String =
    tools.TextNormalization.deburr(s).toLowerCase(Locale.ROOT)
      .replaceAll("[^a-z0-9]+", "-")
      .replaceAll("(^-|-$)", "")
}

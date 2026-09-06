package services.enrichment

import clients.TmdbClient
import models.MovieRecord
import services.movies.{CacheKey, TitleNormalizer}

/**
 * The titles a rating site (Metacritic, Rotten Tomatoes) is asked for one film
 * under, derived once from the row and its TMDB details.
 *
 *   - `linkTitle`  — the primary: TMDB's original title, else the cinema title
 *                    with its programme decoration stripped (`searchQuery`, so a
 *                    "Kino bez barier: Arco (AD)" row asks for just "Arco"; the
 *                    cache key stays decorated so the accessibility screening
 *                    keeps its own row).
 *   - `fallback`   — that stripped cinema title, when it differs from the primary.
 *   - `year`       — TMDB's release year, the slug-suffix / SERP disambiguator.
 *   - `candidates` — the ladder the site is probed under, primary first, then
 *                    TMDB's en-US `title` for a non-English film, then the US
 *                    alternative title for UK/US release-title divergence (HP1:
 *                    TMDB keeps the British title in the en-US locale, but the
 *                    US title from /alternative_titles is the one the sites
 *                    index under). A later title is dropped when it only repeats
 *                    an earlier one, case-insensitively.
 */
final case class RatingSiteTitles(linkTitle: String, fallback: Option[String], year: Option[Int], candidates: Seq[String])

object RatingSiteTitles {
  def derive(key: CacheKey, row: MovieRecord, details: Option[TmdbClient.Details], normalizer: TitleNormalizer): RatingSiteTitles = {
    val cleanLookup = normalizer.searchQuery(key.cleanTitle)
    val linkTitle   = row.originalTitle.getOrElse(cleanLookup)
    val fallback    = if (linkTitle != cleanLookup) Some(cleanLookup) else None
    val englishTitle = details.flatMap(_.englishTitle)
      .filterNot(_.equalsIgnoreCase(linkTitle))
      .filterNot(t => fallback.exists(_.equalsIgnoreCase(t)))
    val usTitle = details.flatMap(_.usTitle)
      .filterNot(_.equalsIgnoreCase(linkTitle))
      .filterNot(t => fallback.exists(_.equalsIgnoreCase(t)))
      .filterNot(t => englishTitle.exists(_.equalsIgnoreCase(t)))
    RatingSiteTitles(linkTitle, fallback, details.flatMap(_.releaseYear), Seq(linkTitle) ++ englishTitle ++ usTitle)
  }
}

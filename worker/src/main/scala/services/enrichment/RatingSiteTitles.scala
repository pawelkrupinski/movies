package services.enrichment

import clients.TmdbClient
import models.MovieRecord
import services.movies.{CacheKey, TitleNormalizer}
import services.resolution.TitleMatch

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

  /** A film a rating site's search page lists: its slug, the title it shows and the year it dates it. */
  trait SearchHit {
    def slug:  String
    def title: String
    def year:  Option[Int]
  }

  /** How strictly one site's search picker takes a hit's year.
   *
   *  @param exactYear    whether an EXACT-title hit dated `hitYear` may be the film of `filmYear`.
   *  @param modifierYear the same for a MODIFIER-suffix hit ("<title> - Re-Release", "<title>: Restored").
   *  @param modifierOnlyWithoutExactTitle when true the modifier hits are a fallback for "the site lists no exact
   *                      title at all", never for "the exact titles were rejected on their year" (Rotten Tomatoes'
   *                      `/m/lalka_1969` leak); when false they also stand in for year-rejected exact titles. */
  final case class SearchHitBar(
    exactYear:                     (Option[Int], Option[Int]) => Boolean,
    modifierYear:                  (Option[Int], Option[Int]) => Boolean,
    modifierOnlyWithoutExactTitle: Boolean
  )

  /** Pick the best search hit conservatively. Two acceptance bars:
   *
   *   1. **Exact title match** (case- and accent-folded) — the query equals the hit's title.
   *   2. **Modifier-suffix match** — the hit's title starts with the query AND the very next non-space character is
   *      a separator (`-`, `:`, `(`, `[`, …), not another word. This catches re-release / restoration / anniversary
   *      variants ("I Vitelloni - Re-Release") while rejecting different films that merely begin with the same word
   *      ("Deaf President Now!" for query "Deaf").
   *
   *  Each bar is year-guarded as the site's [[SearchHitBar]] says. We deliberately do NOT fall back to year-distance
   *  over arbitrary partial matches: search pages list many unrelated films sharing a word, and picking the
   *  year-closest among them produces confident-looking but wrong URLs. Better to store None and let the view
   *  synthesise a search link. Among the accepted hits, year-distance to the requested year breaks ties. */
  def pickBestSearchHit[H <: SearchHit](hits: Seq[H], query: String, year: Option[Int], bar: SearchHitBar): Option[H] = {
    val normalizedQuery = TitleMatch.fold(query)
    if (hits.isEmpty || normalizedQuery.isEmpty) None
    else {
      val exactTitle = hits.filter(h => TitleMatch.exact(h.title, normalizedQuery))
      val exact      = exactTitle.filter(h => bar.exactYear(year, h.year))
      val modifier   = hits.filter(h => TitleMatch.isModifierSuffix(h.title, normalizedQuery)).filter(h => bar.modifierYear(year, h.year))
      val candidates =
        if (exact.nonEmpty) exact
        else if (exactTitle.isEmpty || !bar.modifierOnlyWithoutExactTitle) modifier
        else Seq.empty
      candidates
        .sortBy(h => year.flatMap(y => h.year.map(hy => math.abs(hy - y))).getOrElse(Int.MaxValue))
        .headOption
    }
  }
}

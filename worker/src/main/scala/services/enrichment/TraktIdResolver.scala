package services.enrichment

import services.enrichment.TraktClient.TraktMovie
import tools.TextNormalization

/**
 * Resolves a film's missing TMDB / IMDb id from Trakt, whose search results
 * carry both ids together (see [[TraktClient]]). Business logic only — the
 * infrastructure (HTTP, JSON) lives in TraktClient; this decides what to bind.
 *
 * Two paths, strongest first:
 *   1. **Id bridge** — when a `tt…` IMDb id is already known, Trakt's
 *      `/search/imdb` returns the same film's TMDB id EXACTLY. No corroboration
 *      needed: an id lookup cannot bind the wrong film.
 *   2. **Title search fallback** — when only titles + year are known, accept a
 *      candidate ONLY when its (diacritic-folded) title matches exactly, its
 *      year does not contradict, and it is the LONE such match. Same "never
 *      guess among several" gate as OMDbClient / ImdbClient — Trakt's fuzzy
 *      search would otherwise bind a wrong same-title film.
 */
class TraktIdResolver(trakt: TraktClient) {
  import TraktIdResolver._

  def resolve(imdbId: Option[String], titles: Seq[String], year: Option[Int]): TraktResolution = {
    val known = imdbId.map(_.trim).filter(_.startsWith("tt"))
    // Report the ids of the film Trakt actually RESOLVED — never echo the
    // caller's input id back (that isn't a resolution, and a miss must read as
    // empty, not as "here's the id you already gave me").
    known.flatMap(trakt.findByImdbId).filter(_.tmdbId.isDefined) match {
      case Some(m) => TraktResolution(m.tmdbId, m.imdbId) // 1. exact id bridge
      case None =>                                        // 2. corroborated title search
        val hit = titleSearch(titles, year)
        TraktResolution(hit.flatMap(_.tmdbId), hit.flatMap(_.imdbId))
    }
  }

  /** First title spelling that yields a lone corroborated Trakt match. */
  private def titleSearch(titles: Seq[String], year: Option[Int]): Option[TraktMovie] =
    titles.map(_.trim).filter(_.nonEmpty).distinct.iterator
      .flatMap(t => loneMatch(t, year).iterator)
      .nextOption()

  private def loneMatch(title: String, year: Option[Int]): Option[TraktMovie] = {
    val exact = trakt.search(title, year)
      .filter(m => norm(m.title) == norm(title) && !yearContradicts(m.year, year))
    // Accept only when the exact-title matches agree on a single film.
    val distinct = exact.map(m => (m.tmdbId, m.imdbId)).distinct
    if (distinct.sizeIs == 1) exact.headOption else None
  }
}

object TraktIdResolver {

  /** The ids Trakt could resolve for a film; both None when nothing corroborated. */
  final case class TraktResolution(tmdbId: Option[Int], imdbId: Option[String])

  private def norm(s: String): String =
    TextNormalization.deburr(s).toLowerCase.filter(_.isLetterOrDigit)

  private def yearContradicts(candYear: Option[Int], year: Option[Int]): Boolean =
    (for { y <- year; cy <- candYear } yield math.abs(y - cy) > 1).getOrElse(false)
}

package services.enrichment

import models.CacheKey
import models.MovieRecord
import services.enrichment.SiteRatings.{Found, PageRead}
import services.resolution.{ResolutionCache, ResolutionKeys}
import services.titles.TitleNormalizer

/**
 * Rotten Tomatoes behind the rating-site port: `rottenTomatoesUrl` discovery (slug probe with year-suffix preference,
 * cleanTitle fallback, the lazy `englishTitle` fallback for non-English films) and the Tomatometer read off the page.
 * [[SiteRatings]] runs the flow.
 *
 * Discovery needs TMDB data (release year, English title) that the row alone doesn't carry — TMDB's details are asked
 * ([[RatingSiteFilms]]) only when a row needs its page found.
 */
final class RottenTomatoesRatings(
  films:      RatingSiteFilms,
  rt:         RottenTomatoesClient,
  normalizer: TitleNormalizer,
  // Caches the RT url discovery keyed by (title, fallback, year), so the same
  // film's slug probe runs once for 24h. Passthrough by default (tests).
  rtLinkCache: ResolutionCache = ResolutionCache.passthrough
) extends SiteRatings.Site[Int] {

  val name      = "RT"
  val walkLabel = "RT refresh"

  def find(key: CacheKey, row: MovieRecord): Option[Found[Int]] = {
    val film = films.of(key, row)
    row.tmdbId.fold(findWithoutTmdb(film)) { tmdbId =>
      val titles = film.titles(tmdbId)
      // Cache the whole slug-probe chain keyed by the primary identity, so a cache hit skips the RT HTTP probes
      // entirely. One attempt across all the candidate titles rather than one each: same ladder, same order, but the
      // titles share a fetch memo so a slug an earlier title already probed isn't probed again ("The Sting" and
      // "Sting" both end at /m/sting).
      rtLinkCache.getOrResolve(ResolutionKeys.rt(titles.linkTitle, titles.fallback, titles.year, normalizer)) {
        rt.urlForAny(titles.candidates, titles.fallback, titles.year, film.probeDirectors)
      }.map(Found.Link(_))
    }
  }

  // A film TMDB has no record of: searched under its stripped cinema titles and taken only when
  // the page positively agrees with what the cinemas publish (`TmdbLessRatingLinks`).
  private def findWithoutTmdb(film: RatingSiteFilms.Film): Option[Found[Int]] =
    film.withoutTmdb(rt.urlForAny(_, None, _, _))(url =>
      Option.when(rt.pageFor(url).exists(page => film.corroboratedBy(page.year, page.directors)))(
        Found.Link(url, how = " without TMDB")))

  /** The Tomatometer on `url`'s page, or `Denied` when the page is a DIFFERENT film's.
   *
   *  The page says which film it is about: when it names a year the row's own year positively contradicts, the url is
   *  another film's. Re-resolution alone never fixes this — discovery writes only when it finds a page, so a row with
   *  no RT page of its own kept scoring the namesake's forever: Wanda Jakubowska's "Zaproszenie" (1986) served the
   *  Tomatometer of Olivia Wilde's 2026 film. Only a POSITIVE conflict counts; an undated page is not evidence. */
  def read(key: CacheKey, row: MovieRecord, url: String): PageRead[Int] = {
    val fetched = rt.pageFor(url)
    val film    = films.of(key, row)
    val deniedBy = fetched.flatMap { page =>
      if (film.otherFilmsYear(page.year, page.directors)) Some(s"page names ${page.year.getOrElse("?")}")
      // An UNDATED page is no evidence on year, and RT leaves many undated — so the year guard waved through
      // /m/sacrifice (Umberto Lenzi's 1972 "Sacrifice!") for Romain Gavras's 2026 "Sacrifice" ("Bogaci i martwi").
      // Its credit is not silent.
      else if (film.deniedBy(page.directors))
        Some(s"page credits ${page.directors.mkString(", ")}")
      else None
    }
    deniedBy.fold(PageRead.Scored(fetched.flatMap(_.score)))(PageRead.Denied(_))
  }
}

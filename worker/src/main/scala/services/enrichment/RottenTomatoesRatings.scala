package services.enrichment

import clients.TmdbClient
import models.MovieRecord
import services.enrichment.SiteRatings.{Columns, Found, PageRead}
import services.movies.{CacheKey, LatestTitleYear}
import services.resolution.{ResolutionCache, ResolutionKeys}
import services.titles.TitleNormalizer

/**
 * Rotten Tomatoes behind the rating-site port: `rottenTomatoesUrl` discovery (slug probe with year-suffix preference,
 * cleanTitle fallback, the lazy `englishTitle` fallback for non-English films) and the Tomatometer read off the page.
 * [[SiteRatings]] runs the flow.
 *
 * Discovery needs TMDB data (release year, English title) that the row alone doesn't carry — `tmdb.details(tmdbId)`
 * is asked only when a row needs its page found.
 */
final class RottenTomatoesRatings(
  tmdb:       TmdbClient,
  rt:         RottenTomatoesClient,
  normalizer: TitleNormalizer,
  // Caches the RT url discovery keyed by (title, fallback, year), so the same
  // film's slug probe runs once for 24h. Passthrough by default (tests).
  rtLinkCache: ResolutionCache = ResolutionCache.passthrough
)(using LatestTitleYear) extends SiteRatings.Site[Int] {

  val name      = "RT"
  val walkLabel = "RT refresh"
  val columns: Columns[Int] = RottenTomatoesRatings.Columns

  def find(key: CacheKey, row: MovieRecord): Option[Found[Int]] =
    row.tmdbId.fold(findWithoutTmdb(key, row)) { tmdbId =>
      val titles = RatingSiteTitles.derive(key.cleanTitle, row, tmdb.details(tmdbId), normalizer)
      // Cache the whole slug-probe chain keyed by the primary identity, so a cache hit skips the RT HTTP probes
      // entirely. One attempt across all the candidate titles rather than one each: same ladder, same order, but the
      // titles share a fetch memo so a slug an earlier title already probed isn't probed again ("The Sting" and
      // "Sting" both end at /m/sting).
      rtLinkCache.getOrResolve(ResolutionKeys.rt(titles.linkTitle, titles.fallback, titles.year, normalizer)) {
        rt.urlForAny(titles.candidates, titles.fallback, titles.year, RatingPageIdentity.directorsOf(row, tmdb.directorsFor))
      }.map(Found.Link(_))
    }

  // A film TMDB has no record of: searched under its stripped cinema titles and taken only when
  // the page positively agrees with what the cinemas publish (`TmdbLessRatingLinks`).
  private def findWithoutTmdb(key: CacheKey, row: MovieRecord): Option[Found[Int]] =
    TmdbLessRatingLinks.discover(key, row, normalizer)(rt.urlForAny(_, None, _, _))(url =>
      Option.when(rt.pageFor(url).exists(page => TmdbLessRatingLinks.corroborated(key, row, page.year, page.directors)))(
        Found.Link(url, how = " without TMDB")))

  /** The Tomatometer on `url`'s page, or `Denied` when the page is a DIFFERENT film's.
   *
   *  The page says which film it is about: when it names a year the row's own year positively contradicts, the url is
   *  another film's. Re-resolution alone never fixes this — discovery writes only when it finds a page, so a row with
   *  no RT page of its own kept scoring the namesake's forever: Wanda Jakubowska's "Zaproszenie" (1986) served the
   *  Tomatometer of Olivia Wilde's 2026 film. Only a POSITIVE conflict counts; an undated page is not evidence. */
  def read(key: CacheKey, row: MovieRecord, url: String): PageRead[Int] = {
    val fetched = rt.pageFor(url)
    val deniedBy = fetched.flatMap { page =>
      if (!MetacriticClient.yearsCompatible(key.year, page.year) &&
          !RatingPageIdentity.directorsAgree(row, page.directors, tmdb.directorsFor)) Some(s"page names ${page.year.getOrElse("?")}")
      // An UNDATED page is no evidence on year, and RT leaves many undated — so the year guard waved through
      // /m/sacrifice (Umberto Lenzi's 1972 "Sacrifice!") for Romain Gavras's 2026 "Sacrifice" ("Bogaci i martwi").
      // Its credit is not silent.
      else if (RatingPageIdentity.directorDenies(row, page.directors, tmdb.directorsFor))
        Some(s"page credits ${page.directors.mkString(", ")}")
      else None
    }
    deniedBy.fold(PageRead.Scored(fetched.flatMap(_.score)))(PageRead.Denied(_))
  }
}

object RottenTomatoesRatings {
  val Columns: SiteRatings.Columns[Int] = SiteRatings.Columns[Int](
    noun      = "Tomatometer",
    plural    = "score(s)",
    url       = _.rottenTomatoesUrl,
    score     = _.rottenTomatoes,
    withUrl   = (row, url) => row.copy(rottenTomatoesUrl = Some(url)),
    withScore = (row, score) => row.copy(rottenTomatoes = score),
    withPage  = (row, page) => row.copy(rottenTomatoes = page.score),
    cleared   = _.copy(rottenTomatoesUrl = None, rottenTomatoes = None),
    badge     = score => s"$score%")
}

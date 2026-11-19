package services.enrichment

import clients.TmdbClient
import models.MovieRecord
import services.enrichment.SiteRatings.{Columns, Found, PageRead}
import services.movies.{CacheKey, LatestTitleYear}
import services.resolution.{ResolutionCache, ResolutionKeys}
import services.titles.TitleNormalizer

/**
 * Metacritic behind the rating-site port: `metacriticUrl` discovery (slug probe, cleanTitle fallback, the lazy
 * `englishTitle` fallback for non-English films) and the Metascore read off the page. [[SiteRatings]] runs the flow.
 *
 * Discovery needs TMDB data (release year, English title) that the row alone doesn't carry — `tmdb.details(tmdbId)`
 * is asked only when a row needs its page found, never for a row that already has a link.
 */
final class MetascoreRatings(
  tmdb:       TmdbClient,
  metacritic: MetacriticClient,
  normalizer: TitleNormalizer,
  // Caches the MC url discovery keyed by (title, fallback, year), so the same
  // film's slug probe runs once for 24h. Passthrough by default (tests).
  mcLinkCache: ResolutionCache = ResolutionCache.passthrough
)(using LatestTitleYear) extends SiteRatings.Site[Int] {

  val name      = "Metacritic"
  val walkLabel = "Metascore refresh"
  val columns: Columns[Int] = MetascoreRatings.Columns

  /** A canonical /movie/ URL, probed with TMDB's title data, and the Metascore the probe already parsed off the page
   *  (on a link-cache MISS, where the page was just fetched; on a HIT the flow reads it). */
  def find(key: CacheKey, row: MovieRecord): Option[Found[Int]] =
    row.tmdbId.fold(findWithoutTmdb(key, row)) { tmdbId =>
      val titles = RatingSiteTitles.derive(key.cleanTitle, row, tmdb.details(tmdbId), normalizer)
      // The link cache stores the URL only; the Metascore the resolving probe parsed is threaded out-of-band, set ONLY
      // when the resolve closure actually ran.
      var freshScore: Option[Int] = None
      val url = mcLinkCache.getOrResolve(ResolutionKeys.mc(titles.linkTitle, titles.fallback, titles.year, normalizer)) {
        // One attempt across all the candidate titles rather than one each: same ladder, same order, but the titles
        // share a fetch memo so a slug an earlier title already probed isn't probed again ("The Sting" and "Sting"
        // both end at /movie/sting).
        //
        // The film's own director — the one thing that tells two same-titled, same-year films apart — from
        // `directorsFor`, NOT the row's `Tmdb` slot: the slot keeps only the localised credit, so an anime directed by
        // 秋本賢一郎 would never meet Metacritic's "Kenichiro Akimoto"; `directorsFor` returns the localised AND
        // native-script spellings.
        val resolved = metacritic.resolveAcross(titles.candidates, titles.fallback, titles.year, tmdb.directorsFor(tmdbId))
        freshScore = resolved.flatMap(_.metascore)
        resolved.map(_.url)
      }
      url.map(Found.Link(_, freshScore))
    }

  // A film TMDB has no record of: searched under its stripped cinema titles and taken only when
  // the page positively agrees with what the cinemas publish (`TmdbLessRatingLinks`).
  private def findWithoutTmdb(key: CacheKey, row: MovieRecord): Option[Found[Int]] =
    TmdbLessRatingLinks.discover(key, row, normalizer)(metacritic.resolveAcross(_, None, _, _))(resolved =>
      metacritic.pageFor(resolved.url).filter(page => TmdbLessRatingLinks.corroborated(key, row, page.year, page.directors))
        .map(page => Found.Link(resolved.url, page.metascore, " without TMDB")))

  /** The Metascore on `url`'s page, or `Denied` when the page credits a director the row's film does not.
   *
   *  Deliberately NOT year-checked, unlike RT and Filmweb: Metacritic's `datePublished` is a US RELEASE date, so a
   *  restoration carries the modern one — /movie/angels-egg-1985 is Oshii's 1985 film and publishes 2025. The slug
   *  ladder already year-guards what it probes; a URL from the search fallback was accepted on the SERP's year.
   *
   *  Its CREDIT is checked, though: prod's "Bogaci i martwi" (Romain Gavras, 2026) held /movie/sacrifice-2021, and
   *  nothing else would ever replace it (see `RatingPageIdentity`). */
  def read(key: CacheKey, row: MovieRecord, url: String): PageRead[Int] = {
    val page = metacritic.pageFor(url)
    if (page.exists(p => RatingPageIdentity.directorDenies(row, p.directors, tmdb.directorsFor)))
      PageRead.Denied(s"page credits ${page.toSeq.flatMap(_.directors).mkString(", ")}")
    else PageRead.Scored(page.flatMap(_.metascore))
  }
}

object MetascoreRatings {
  val Columns: SiteRatings.Columns[Int] = SiteRatings.Columns[Int](
    noun      = "metascore",
    plural    = "score(s)",
    url       = _.metacriticUrl,
    score     = _.metascore,
    withUrl   = (row, url) => row.copy(metacriticUrl = Some(url)),
    withScore = (row, score) => row.copy(metascore = score),
    withPage  = (row, page) => row.copy(metascore = page.score),
    cleared   = _.copy(metacriticUrl = None, metascore = None),
    badge     = _.toString)
}

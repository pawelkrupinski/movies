package services.enrichment

import clients.TmdbClient
import models.{EmbeddedYear, Filmweb, MovieRecord, Source, SourceData}
import services.enrichment.SiteRatings.{Columns, Found, Page, PageRead}
import services.movies.{CacheKey, LatestTitleYear}
import services.resolution.{ResolutionCache, ResolutionKeys}
import services.titles.TitleNormalizer

/**
 * Filmweb behind the rating-site port. Unlike Metacritic and RT, Filmweb is a full enrichment source: a page found or
 * read whole contributes a content slot (original title, year, directors, Polish genres and plot) besides its rating,
 * and that slot drives re-resolution of the other sources when TMDB/IMDb are absent. [[SiteRatings]] runs the flow.
 *
 * Discovery needs TMDB data (English / original title, director credits) that the row alone may not carry —
 * `tmdb.details` and `tmdb.directorsFor` are asked only when a row needs its page found.
 *
 * Filmweb has tighter rate limits than the other sites ("5 workers comfortable, more risks soft-blocks"): its walk is
 * capped at 5.
 */
final class FilmwebRatings(
  tmdb:       TmdbClient,
  filmweb:    FilmwebClient,
  normalizer: TitleNormalizer,
  // Caches the Filmweb url discovery keyed by (title, year, fallback, directors),
  // so the same film's search + /info + /preview probes run once for 24h. Only
  // the url is cached; on a cache hit the rating + genres are rebuilt from the
  // url. Passthrough by default (tests).
  filmwebLinkCache: ResolutionCache = ResolutionCache.passthrough,
  // Called whenever we set (or confirm) a filmwebUrl on a row that still has
  // no imdbId — signals the resolver to attempt the Wikidata fallback.
  // Args: (title, year, searchTitle) matching ImdbIdMissing. No-op by default.
  onImdbIdMissing: (String, Option[Int], String) => Unit = (_, _, _) => ()
)(using LatestTitleYear) extends SiteRatings.Site[Double] {

  val name      = "Filmweb"
  val walkLabel = "Filmweb refresh"
  val columns: Columns[Double] = FilmwebRatings.Columns

  // Filmweb soft-blocks a datacenter IP past a handful of concurrent requests, so its walk runs lower than the others.
  override val walkConcurrency: Int = 5

  /** The film's page, read whole by the lookup (search → /info per candidate → /preview per candidate when verifying
   *  directors → rating). */
  def find(key: CacheKey, row: MovieRecord): Option[Found[Double]] =
    (if (row.tmdbId.isEmpty && row.filmwebUrl.isEmpty) findWithoutTmdb(key, row) else findWithTmdbHints(key, row))
      .map(info => Found.Whole(info.url, FilmwebRatings.page(info)))

  /** The page at a stored `url`. A Filmweb URL names its film's year in the path, and when that positively contradicts
   *  the row's own year the URL is a DIFFERENT film's — `Denied`, nothing fetched. Re-resolution alone never fixes
   *  this: discovery writes only when it finds a page, so a row with no Filmweb page of its own kept serving the
   *  namesake's rating forever — Wanda Jakubowska's "Zaproszenie" (1986) carried /film/Zaproszenie-2026-10109168. The
   *  suspect page's own credit (its slot was written off this URL) is weighed against the TRUSTED side only — TMDB's
   *  crew and the cinemas' credits. An undated URL is not evidence.
   *
   *  A stored URL the row holds no Filmweb slot for is read whole, as a discovery is: a page the projection linked
   *  (the resolver's Filmweb fallback, `ProjectionDrafting.heldLinks`, drops the slot of the page it replaces) arrives
   *  as a URL alone, and a rating-only read never fills its content -- PL "Flow" (2024) showed no original title,
   *  genres or plot, which the same page found by a title search first had stored. */
  def read(key: CacheKey, row: MovieRecord, url: String): PageRead[Double] = {
    val pageCredit = row.data.get(Filmweb).toSeq.flatMap(_.director).toSet
    if (!MetacriticClient.yearsCompatible(key.year, FilmwebClient.yearInUrl(url)) &&
        !RatingPageIdentity.directorsAgree(row, pageCredit, tmdb.directorsFor))
      PageRead.Denied(s"URL names ${FilmwebClient.yearInUrl(url).getOrElse("?")}")
    else if (!row.data.contains(Filmweb)) filmweb.detailsFor(url).fold(PageRead.Scored(None))(info => PageRead.Read(FilmwebRatings.page(info)))
    else walkRead(key, row, url)
  }

  /** The walk reads the rating alone, one request a row. */
  override def walkRead(key: CacheKey, row: MovieRecord, url: String): PageRead[Double] =
    PageRead.Scored(filmweb.ratingFor(url).map(RatingDisplay.oneDecimal))

  // A film TMDB has no record of takes its IMDb id from the identity resolver's fallback source, never from a title search.
  override def pageTaken(key: CacheKey, row: MovieRecord): Unit =
    if (row.imdbId.isEmpty && row.tmdbId.isDefined)
      onImdbIdMissing(key.cleanTitle, key.year, row.originalTitle.getOrElse(normalizer.apiQuery(key.cleanTitle)))

  // A film TMDB has no record of: each stripped cinema title searched in turn, and a page taken only
  // when it positively agrees with what the cinemas publish (`TmdbLessRatingLinks`).
  private def findWithoutTmdb(key: CacheKey, row: MovieRecord): Option[FilmwebClient.FilmwebInfo] =
    Option.when(TmdbLessRatingLinks.checkable(key, row))(()).flatMap { _ =>
      val directors = services.movies.TmdbLessEligibility.directorsOf(row)
      TmdbLessRatingLinks.titlesOf(key, row, normalizer).iterator
        .flatMap(title => filmweb.lookup(title, TmdbLessRatingLinks.yearOf(key, row), None, directors, None))
        .find(info => TmdbLessRatingLinks.corroborated(key, row, info.year, info.directors.toSet))
    }

  // Passes TMDB's originalTitle / englishTitle as `fallback` so non-Polish films whose cinema-reported title doesn't
  // surface a Filmweb hit still resolve, and the union of TMDB credits + every cinema's reported director as
  // `directors` so same-titled films across years disambiguate.
  private def findWithTmdbHints(key: CacheKey, row: MovieRecord): Option[FilmwebClient.FilmwebInfo] = {
    // Strip accessibility-programme decoration before hitting Filmweb —
    // "Kino bez barier: Freak Show (AD)" queries upstream as just
    // "Freak Show". Cache key keeps the full form so this row stays
    // distinct from the DKF / regular screening of the same film.
    val linkTitle = normalizer.searchQuery(key.cleanTitle)
    val details   = row.tmdbId.flatMap(tmdb.details)
    val fallback  = row.originalTitle
      .orElse(details.flatMap(_.englishTitle))
      .filterNot(_.equalsIgnoreCase(linkTitle))
    val directors = row.tmdbId.map(tmdb.directorsFor).getOrElse(Set.empty) ++ row.evidence.directors.toSet
    // A yearless retrospective ("Konwicki: Lawa (1989)") carries no scraped year,
    // so the Filmweb search had nothing to gate a same-title collision against and
    // took the unrelated "Lawa"/orig "Lava" (2014). Read a delimited "(YYYY)" (or
    // [YYYY]/{YYYY}/<YYYY>/"- YYYY") off the row's raw cinema titles as the lookup
    // year — the SAME deterministic hint `resolveTmdbId` uses (`EmbeddedYear`), fed
    // here so the Filmweb side gets the same disambiguation. Falls back to the
    // scraped key year.
    val effectiveYear = key.year.orElse(EmbeddedYear.ofAll(key.cleanTitle +: row.evidence.titles.toSeq, LatestTitleYear.current))
    // TMDB's Polish blurb (same language as Filmweb's `plot`) breaks a same-year
    // same-title tie inside `lookup`; None when TMDB hasn't resolved a synopsis.
    val referenceSynopsis = row.data.get(models.Tmdb).flatMap(_.synopsis)
      .orElse(row.retainedSynopses.get(models.Tmdb))

    // Cache the url discovery (the expensive search + /info + /preview probes)
    // keyed by the Filmweb hints. `fresh` carries the full FilmwebInfo on a
    // cache MISS so the rating + genres are kept exactly as before; on a cache
    // HIT only the url is known, so rebuild the rating + genres from it.
    var fresh: Option[FilmwebClient.FilmwebInfo] = None
    val cachedUrl = filmwebLinkCache.getOrResolve(ResolutionKeys.filmweb(linkTitle, effectiveYear, fallback, directors, normalizer)) {
      val info = filmweb.lookup(linkTitle, effectiveYear, fallback, directors, referenceSynopsis)
      fresh = info
      info.map(_.url)
    }
    cachedUrl.map(url => fresh.orElse(filmweb.detailsFor(url)).getOrElse(FilmwebClient.FilmwebInfo(url, filmweb.ratingFor(url))))
  }
}

object FilmwebRatings {

  /** A Filmweb read as a page: its rating at the precision the badge shows (`%.1f`: a sub-decimal vote drift the user
   *  can't see isn't a change — see RatingDisplay), and its content slot — the Filmweb counterpart of
   *  [[ImdbRatings.makeSlot]]: originalTitle, year, directors, Polish genres, Polish plot. A slot with no content at
   *  all is none, and drops the row's. */
  private def page(info: FilmwebClient.FilmwebInfo): Page[Double] = {
    val slot = SourceData(
      originalTitle = info.originalTitle,
      releaseYear   = info.year,
      director      = info.directors,
      genres        = info.genres,
      synopsis      = info.plot
    )
    val hasContent = slot.originalTitle.isDefined || slot.releaseYear.isDefined ||
                     slot.director.nonEmpty || slot.genres.nonEmpty || slot.synopsis.isDefined
    Page(info.rating.map(RatingDisplay.oneDecimal), Option.when(hasContent)(slot))
  }

  val Columns: SiteRatings.Columns[Double] = SiteRatings.Columns[Double](
    noun      = "rating",
    plural    = "rating(s)",
    url       = _.filmwebUrl,
    score     = _.filmwebRating,
    withUrl   = (row, url) => row.copy(filmwebUrl = Some(url)),
    withScore = (row, rating) => row.copy(filmwebRating = rating),
    withPage  = (row, page) => row.copy(filmwebRating = page.score,
      data = page.slot.fold(row.data - (Filmweb: Source))(slot => row.data + ((Filmweb: Source) -> slot))),
    cleared   = row => row.copy(filmwebUrl = None, filmwebRating = None, data = row.data - (Filmweb: Source)),
    badge     = RatingDisplay.label)
}

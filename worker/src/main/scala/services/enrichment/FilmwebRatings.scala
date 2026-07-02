package services.enrichment

import clients.TmdbClient
import models.{Filmweb, Source, SourceData}
import services.movies.{CacheKey, MovieCache, MovieService}
import services.resolution.{ResolutionCache, ResolutionKeys}
import tools.BoundedParallel

import java.util.concurrent.atomic.AtomicInteger
import scala.util.{Failure, Success, Try}

/**
 * Filmweb data maintenance — the Filmweb counterpart of `ImdbRatings`,
 * `RottenTomatoesRatings`, and `MetascoreRatings`.
 *
 * Two refresh paths (both driven by the queue):
 *   1. **Per-row refresh** (`refreshOne`): the `RatingHandler` fetches Filmweb
 *      data for one resolved row.
 *   2. **Full-corpus refresh** (`refreshAll`): the operator-triggered bulk
 *      refresh. For rows that already have a `filmwebUrl`, the walk does the
 *      cheap rating-only lookup (one HTTP); for rows without a URL it does the
 *      full `filmweb.lookup` (search + info + optionally preview + rating).
 *
 * URL resolution needs TMDB data (English / original title, director credits)
 * that the MovieRecord row alone may not carry — we hit `tmdb.details` and
 * `tmdb.directorsFor` lazily for rows that need URL discovery, and never for
 * rows whose canonical Filmweb URL is already stored.
 *
 * Shared entry points live in [[CacheRefresher]].
 *
 * Filmweb has tighter rate limits than the other services (CLAUDE.md notes
 * "5 workers comfortable, more risks soft-blocks"); we cap its walk at 5.
 */
class FilmwebRatings(
  cache:   MovieCache,
  tmdb:    TmdbClient,
  filmweb: FilmwebClient,
  // Caches the Filmweb url discovery keyed by (title, year, fallback, directors),
  // so the same film's search + /info + /preview probes run once for 24h. Only
  // the url is cached; on a cache hit the rating + genres are rebuilt from the
  // url. Passthrough by default (tests).
  filmwebLinkCache: ResolutionCache = ResolutionCache.passthrough,
  // Called whenever we set (or confirm) a filmwebUrl on a row that still has
  // no imdbId — signals the resolver to attempt the Wikidata fallback.
  // Args: (title, year, searchTitle) matching ImdbIdMissing. No-op by default.
  onImdbIdMissing: (String, Option[Int], String) => Unit = (_, _, _) => (),
  cadenceRecorder: (CacheKey, Option[Int], Option[String]) => Unit = (_, _, _) => (),
  deadbandConfirmationsFor: (CacheKey, Option[Int]) => Int = (_, _) => RatingDeadband.Off
) extends CacheRefresher(cache, cadenceRecorder, deadbandConfirmationsFor) {

  override protected def sourceName: String = "Filmweb"

  /** Audit-and-fix the stored `filmwebUrl` for `(title, year)` against the
   *  tightened `FilmwebClient.lookup`. Re-resolves regardless of whether a
   *  URL is already stored, writes back any change to the cache (which
   *  write-throughs to Mongo), and returns a classification so the caller
   *  (typically `scripts.FilmwebUrlAudit`) can summarise the run.
   *
   *  Outcomes:
   *    - Kept       — re-resolved to the same URL; rating refreshed if
   *                   Filmweb returned a fresh value.
   *    - Corrected  — re-resolved to a DIFFERENT canonical URL; URL +
   *                   rating both replaced.
   *    - Dropped    — no candidate clears the tightened bar; URL + rating
   *                   both cleared.
   *    - NoUrl      — row has no filmwebUrl and the re-resolve didn't find
   *                   one either; nothing to do.
   *
   *  Backfill semantics — required by CLAUDE.md when ingestion logic
   *  changes (the old `lookup` had no title acceptance bar, so the DB holds
   *  stale URLs pointing at unrelated films). Public on the ratings class
   *  so the audit script doesn't need cache visibility. */
  def auditOneSync(title: String, year: Option[Int]): FilmwebRatings.AuditOutcome = {
    val key = cache.keyOf(title, year)
    cache.get(key) match {
      case None => FilmwebRatings.NoUrl
      case Some(e) =>
        val resolved = resolveUrl(key, e)
        (e.filmwebUrl, resolved) match {
          case (Some(before), Some(fw)) if fw.url == before =>
            if (fw.rating.isDefined && fw.rating != e.filmwebRating)
              cache.putIfPresent(key, _.copy(filmwebRating = fw.rating))
            FilmwebRatings.Kept(before)
          case (_, Some(fw)) =>
            val before = e.filmwebUrl
            cache.putIfPresent(key, r =>
              r.copy(filmwebUrl = Some(fw.url), filmwebRating = fw.rating, data = withFilmwebGenres(r.data, fw.genres)))
            before match {
              case Some(b) => FilmwebRatings.Corrected(b, fw.url)
              case None    => FilmwebRatings.Kept(fw.url) // first-time discovery counts as Kept-like.
            }
          case (Some(before), None) =>
            cache.putIfPresent(key, r =>
              r.copy(filmwebUrl = None, filmwebRating = None, data = r.data - (Filmweb: Source)))
            FilmwebRatings.Dropped(before)
          case (None, None) =>
            FilmwebRatings.NoUrl
        }
    }
  }

  // Look up Filmweb data for the row, write back any changes. Two paths:
  //   - Row already has filmwebUrl → cheap: rating-only refresh via the
  //     URL's id. Skips the search/info round-trips that the original
  //     resolution paid for.
  //   - Row doesn't have filmwebUrl → expensive: full lookup with TMDB-
  //     derived fallback title + director set (search → /info per candidate
  //     → /preview per candidate when verifying directors → rating).
  // Per-row failures are swallowed (network blip, Filmweb soft-block); the
  // next refresh tries again.
  protected def refreshOne(key: CacheKey): Option[String] =
    cache.get(key).flatMap { e =>
      e.filmwebUrl match {
        case Some(url) => refreshRatingFromUrl(key, e, url)
        case None      => resolveAndPersistUrl(key, e)
      }
    }

  // Returns the new displayed rating (badge text) if it changed, else None.
  private def refreshRatingFromUrl(key: CacheKey, e: models.MovieRecord, url: String): Option[String] = {
    val label = s"'${key.cleanTitle}' (${key.year.getOrElse("?")})"
    val change = Try(filmweb.ratingFor(url)).toOption.flatten match {
      case Some(rating) =>
        // Store at the precision the badge shows (`%.1f`): a sub-decimal vote
        // drift the user can't see isn't a change — see RatingDisplay. A change
        // that clears the deadband (a rounding-boundary blip that reverts next
        // tick is held) — see ratingSettled.
        val rounded = RatingDisplay.oneDecimal(rating)
        val commit  = ratingSettled(key, e.tmdbId, e.filmwebRating.map(RatingDisplay.label), Some(RatingDisplay.label(rounded)))
        logger.info(s"Filmweb: $label $url → rating $rounded" +
          (if (commit) s" (was ${e.filmwebRating.getOrElse("—")})" else " (unchanged or held)"))
        if (commit) { cache.putIfPresent(key, _.copy(filmwebRating = Some(rounded))); Some(RatingDisplay.label(rounded)) }
        else None
      case None =>
        logger.info(s"Filmweb: $label $url → no rating on page")
        None
    }
    if (e.imdbId.isEmpty)
      onImdbIdMissing(key.cleanTitle, key.year, e.originalTitle.getOrElse(MovieService.apiQuery(key.cleanTitle)))
    change
  }

  // Full URL discovery — only called when the row has no stored filmwebUrl.
  // Passes TMDB's originalTitle / englishTitle as `fallback` so non-Polish
  // films whose cinema-reported title doesn't surface a Filmweb hit still
  // resolve, and the union of TMDB credits + every cinema's reported director
  // as `directors` so same-titled films across years disambiguate.
  // Returns the new displayed rating (badge text) if it changed, else None (a
  // first discovery that lands a rating counts as a change: None → Some).
  private def resolveAndPersistUrl(key: CacheKey, e: models.MovieRecord): Option[String] = {
    val label = s"'${key.cleanTitle}' (${key.year.getOrElse("?")})"
    resolveUrl(key, e) match {
      case Some(fw) =>
        val changed = fw.rating != e.filmwebRating   // fw.rating is display-rounded by resolveUrl
        logger.info(s"Filmweb: $label → URL discovered ${fw.url} rating=${fw.rating.getOrElse("—")}")
        cache.putIfPresent(key, r =>
          r.copy(filmwebUrl = Some(fw.url), filmwebRating = fw.rating, data = withFilmwebGenres(r.data, fw.genres)))
        if (e.imdbId.isEmpty)
          onImdbIdMissing(key.cleanTitle, key.year, e.originalTitle.getOrElse(MovieService.apiQuery(key.cleanTitle)))
        if (changed) fw.rating.map(RatingDisplay.label) else None
      case None =>
        logger.info(s"Filmweb: $label → no match")
        None
    }
  }

  /** Merge freshly-resolved Filmweb genres into the per-source slot map.
   *  Empty genres → drop the Filmweb slot entirely (don't store a slot that
   *  contributes nothing); non-empty genres → set/replace the slot. */
  private def withFilmwebGenres(data: Map[Source, SourceData], genres: Seq[String]): Map[Source, SourceData] =
    if (genres.isEmpty) data - (Filmweb: Source)
    else data + ((Filmweb: Source) -> SourceData(genres = genres))

  // Pure re-resolve — never writes. Shared by `resolveAndPersistUrl` (production
  // URL-discovery path) and `auditOneSync` (one-off backfill that compares
  // the freshly-resolved URL against what's already stored).
  private def resolveUrl(key: CacheKey, e: models.MovieRecord): Option[FilmwebClient.FilmwebInfo] = {
    // Strip accessibility-programme decoration before hitting Filmweb —
    // "Kino bez barier: Freak Show (AD)" queries upstream as just
    // "Freak Show". Cache key keeps the full form so this row stays
    // distinct from the DKF / regular screening of the same film.
    val linkTitle = MovieService.searchQuery(key.cleanTitle)
    val details   = e.tmdbId.flatMap(tmdb.details)
    val fallback  = e.originalTitle
      .orElse(details.flatMap(_.englishTitle))
      .filterNot(_.equalsIgnoreCase(linkTitle))
    val tmdbDirectors   = e.tmdbId.map(tmdb.directorsFor).getOrElse(Set.empty)
    val cinemaDirectors = e.cinemaData.values.flatMap(_.director).toSet
    val directors       = tmdbDirectors ++ cinemaDirectors
    // TMDB's Polish blurb (same language as Filmweb's `plot`) breaks a same-year
    // same-title tie inside `lookup`; None when TMDB hasn't resolved a synopsis.
    val referenceSynopsis = e.data.get(models.Tmdb).flatMap(_.synopsis)
      .orElse(e.retainedSynopses.get(models.Tmdb))

    // Cache the url discovery (the expensive search + /info + /preview probes)
    // keyed by the Filmweb hints. `fresh` carries the full FilmwebInfo on a
    // cache MISS so the rating + genres are kept exactly as before; on a cache
    // HIT only the url is known, so rebuild the rating + genres from it.
    var fresh: Option[FilmwebClient.FilmwebInfo] = None
    val cachedUrl = filmwebLinkCache.getOrResolve(ResolutionKeys.filmweb(linkTitle, key.year, fallback, directors)) {
      val info = Try(filmweb.lookup(linkTitle, key.year, fallback, directors, referenceSynopsis)).toOption.flatten
      fresh = info
      info.map(_.url)
    }
    cachedUrl.map { url =>
      val info = fresh.getOrElse(FilmwebClient.FilmwebInfo(url, filmweb.ratingFor(url), filmweb.genresFor(url)))
      // Persist at the precision the badge shows, here at the single seam every
      // discovery + audit caller funnels through.
      info.copy(rating = info.rating.map(RatingDisplay.oneDecimal))
    }
  }

  // ── Full-corpus walk ───────────────────────────────────────────────────────

  // Filmweb soft-blocks a datacenter IP past a handful of concurrent requests
  // (see CLAUDE.md), so cap its parallel walk lower than the other sources.
  override protected def refreshConcurrency: Int = 5

  /** Walk every cached row and refresh its Filmweb data. Rows with a URL
   *  get the cheap rating-only refresh; rows without one get the expensive
   *  full-lookup. The latter group is small in practice — only films
   *  Filmweb didn't surface at first enrichment. */
  private[services] def refreshAll(): Unit = {
    val snapshot  = cache.entries
    val startedAt = System.currentTimeMillis()
    val (withUrl, missingUrl) = snapshot.partition { case (_, e) => e.filmwebUrl.isDefined }
    logger.info(s"Filmweb refresh: starting tick over ${snapshot.size} cached row(s) " +
                s"(${withUrl.size} with URL → rating-only, ${missingUrl.size} without → full lookup).")
    val changed       = new AtomicInteger(0)
    val failed        = new AtomicInteger(0)
    val urlDiscovered = new AtomicInteger(0)

    BoundedParallel.foreach("Filmweb-refresh-rating", withUrl, refreshConcurrency) { case (key, enrichment) =>
      val url = enrichment.filmwebUrl.get
      Try(filmweb.ratingFor(url).map(RatingDisplay.oneDecimal)) match {
        case Success(fresh) if fresh != enrichment.filmwebRating =>
          logger.debug(s"Filmweb refresh: ${key.cleanTitle} $url ${enrichment.filmwebRating.getOrElse("—")} → ${fresh.getOrElse("—")}")
          cache.putIfPresent(key, _.copy(filmwebRating = fresh))
          fresh.foreach(r => recordCadenceChange(key, enrichment.tmdbId, Some(RatingDisplay.label(r))))
          changed.incrementAndGet()
        case Success(_) => ()
        case Failure(exception) =>
          failed.incrementAndGet()
          logger.debug(s"Filmweb refresh: $url lookup failed: ${exception.getMessage}")
      }
    }

    BoundedParallel.foreach("Filmweb-refresh-discover", missingUrl, refreshConcurrency) { case (key, enrichment) =>
      Try(resolveAndPersistUrl(key, enrichment)) match {
        case Success(reported) =>
          reported.foreach(v => recordCadenceChange(key, enrichment.tmdbId, Some(v)))
          // urlDiscovered: re-read the cache to see if the helper actually
          // stored a URL. Cheap (single Caffeine lookup) and avoids leaking
          // the resolved Option through the helper's API.
          if (cache.get(key).exists(_.filmwebUrl.isDefined && !enrichment.filmwebUrl.isDefined)) urlDiscovered.incrementAndGet()
        case Failure(exception) =>
          failed.incrementAndGet()
          logger.debug(s"Filmweb refresh: ${key.cleanTitle} full-lookup failed: ${exception.getMessage}")
      }
    }

    val took = System.currentTimeMillis() - startedAt
    logger.info(s"Filmweb refresh: tick done in ${took}ms — ${changed.get} rating(s) changed, " +
                s"${urlDiscovered.get} URL(s) newly discovered, ${failed.get} failed.")
  }

}

object FilmwebRatings {
  /** Outcome of a single `auditOneSync` call. The summary stats in the
   *  `FilmwebUrlAudit` script are derived by counting these. */
  sealed trait AuditOutcome
  /** Stored URL re-resolves to the same canonical id (good). */
  final case class Kept(url: String) extends AuditOutcome
  /** Stored URL re-resolves to a DIFFERENT canonical id — fixed in place. */
  final case class Corrected(before: String, after: String) extends AuditOutcome
  /** Stored URL no longer passes the title + director bar — cleared. */
  final case class Dropped(before: String) extends AuditOutcome
  /** Row had no `filmwebUrl` and re-resolve didn't find one either. */
  case object NoUrl extends AuditOutcome
}

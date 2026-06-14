package services.enrichment

import clients.TmdbClient
import models.{Filmweb, Source, SourceData}
import services.movies.{CacheKey, MovieCache, MovieService}
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
  filmweb: FilmwebClient
) extends CacheRefresher(cache) {

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
  protected def refreshOne(key: CacheKey): Unit =
    cache.get(key).foreach { e =>
      e.filmwebUrl match {
        case Some(url) => refreshRatingFromUrl(key, e, url)
        case None      => resolveAndPersistUrl(key, e)
      }
    }

  private def refreshRatingFromUrl(key: CacheKey, e: models.MovieRecord, url: String): Unit = {
    val label = s"'${key.cleanTitle}' (${key.year.getOrElse("?")})"
    Try(filmweb.ratingFor(url)).toOption.flatten match {
      case Some(rating) =>
        logger.info(s"Filmweb: $label $url → rating $rating" +
          (if (e.filmwebRating.contains(rating)) " (unchanged)" else s" (was ${e.filmwebRating.getOrElse("—")})"))
        if (!e.filmwebRating.contains(rating)) cache.putIfPresent(key, _.copy(filmwebRating = Some(rating)))
      case None =>
        logger.info(s"Filmweb: $label $url → no rating on page")
    }
  }

  // Full URL discovery — only called when the row has no stored filmwebUrl.
  // Passes TMDB's originalTitle / englishTitle as `fallback` so non-Polish
  // films whose cinema-reported title doesn't surface a Filmweb hit still
  // resolve, and the union of TMDB credits + every cinema's reported director
  // as `directors` so same-titled films across years disambiguate.
  private def resolveAndPersistUrl(key: CacheKey, e: models.MovieRecord): Unit = {
    val label = s"'${key.cleanTitle}' (${key.year.getOrElse("?")})"
    resolveUrl(key, e) match {
      case Some(fw) =>
        logger.info(s"Filmweb: $label → URL discovered ${fw.url} rating=${fw.rating.getOrElse("—")}")
        cache.putIfPresent(key, r =>
          r.copy(filmwebUrl = Some(fw.url), filmwebRating = fw.rating, data = withFilmwebGenres(r.data, fw.genres)))
      case None =>
        logger.info(s"Filmweb: $label → no match")
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
    val linkTitle = MovieService.apiQuery(key.cleanTitle)
    val details   = e.tmdbId.flatMap(tmdb.details)
    val fallback  = e.originalTitle
      .orElse(details.flatMap(_.englishTitle))
      .filterNot(_.equalsIgnoreCase(linkTitle))
    val tmdbDirectors   = e.tmdbId.map(tmdb.directorsFor).getOrElse(Set.empty)
    val cinemaDirectors = e.cinemaData.values.flatMap(_.director).toSet
    val directors       = tmdbDirectors ++ cinemaDirectors

    Try(filmweb.lookup(linkTitle, key.year, fallback, directors)).toOption.flatten
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
      Try(filmweb.ratingFor(url)) match {
        case Success(fresh) if fresh != enrichment.filmwebRating =>
          logger.debug(s"Filmweb refresh: ${key.cleanTitle} $url ${enrichment.filmwebRating.getOrElse("—")} → ${fresh.getOrElse("—")}")
          cache.putIfPresent(key, _.copy(filmwebRating = fresh))
          changed.incrementAndGet()
        case Success(_) => ()
        case Failure(exception) =>
          failed.incrementAndGet()
          logger.debug(s"Filmweb refresh: $url lookup failed: ${exception.getMessage}")
      }
    }

    BoundedParallel.foreach("Filmweb-refresh-discover", missingUrl, refreshConcurrency) { case (key, enrichment) =>
      Try(resolveAndPersistUrl(key, enrichment)) match {
        case Success(_) =>
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

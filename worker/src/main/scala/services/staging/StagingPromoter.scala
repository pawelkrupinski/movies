package services.staging

import models.{MovieRecord, Tmdb}
import play.api.Logging
import services.cinemas.DetailEnricher
import services.movies.{MovieRecordMerge, MovieService, TitleNormalizer}

/**
 * Incubates newcomer films sitting in `pending_movies` through the SAME
 * enrichment steps, in the SAME order, the live `movies` pipeline runs — just
 * pointed at staging rows. It works PER FILM (grouped by `sanitize(title)`,
 * across every year-variant), resolving once and folding per year:
 *
 *   1. DETAIL first (for `DetailEnricher` cinemas): fetch each cinema's per-film
 *      detail page and merge it into that cinema's staging row, exactly like
 *      `EnrichDetailsHandler` does for `movies`. Done for EVERY cinema of the
 *      film BEFORE resolving, so the resolved row carries every cinema's cast/
 *      synopsis + director/originalTitle hints — the reason detail runs first.
 *   2. TMDB resolve ONCE per film, over the UNION of every cinema's hints at the
 *      film's best year — a hit writes tmdbId + the Tmdb slot onto every row, a
 *      definitive miss sets `tmdbNoMatch`.
 *   3. On conclusion, publish `StagingFilmEnriched(title)` so the folder graduates
 *      the whole `sanitize(title)` group into `movies` and deletes its staging
 *      rows. (Published once per concluded year; the fold is group-scoped and
 *      idempotent, so a re-fire just re-settles the same group.)
 *
 * Resolution is per-FILM, not per-`(title,year)`-variant: a film whose cinemas
 * disagree on the year (some yearless, some at the production year) must resolve
 * with the UNION of every cinema's director/originalTitle hints — the same hints
 * the direct path's scrape-redirect gives it by merging the yearless + yeared
 * cinemas onto one row before TMDB runs. Resolving each year alone starved the
 * main variant of hints and made it miss. The fold is then group-scoped
 * (`StagingFold.planGroup`): the production-year and release-year rows collapse
 * into one `movies` row via the same `canonicalizeBySanitize` ±1 settle the direct
 * path relies on, run inside the fold rather than by a separate pass.
 *
 * `resolveStaging` is `MovieService.resolveStagingRecord`, injected as a function
 * so the promoter depends on the abstraction, not the whole service.
 */
class StagingPromoter(
  stagingRepository:    StagingRepository,
  enrichers:      Seq[DetailEnricher],
  resolveStaging: (String, Option[Int], MovieRecord) => Option[MovieRecord],
  recoverImdbId:  (String, Option[Int]) => Option[String],
  onConcluded:    StagingRecord => Unit
) extends Logging {

  /** Run one incubation pass over every staging FILM (grouped by `sanitize`,
   *  across all its year-variants). Returns how many films concluded this pass
   *  (for logging). */
  def runOnce(): Int =
    stagingRepository.findAll()
      .groupBy(r => TitleNormalizer.sanitize(r.title))
      .valuesIterator
      .count(promoteFilm)

  /** Advance ONE staging row by promoting its whole film. Exposed for
   *  single-row tests; `runOnce` groups by `sanitize` and calls `promoteFilm`. */
  def promote(row: StagingRecord): Boolean = promoteFilm(Seq(row))

  /** Advance ONE film (all its per-cinema, ALL-year staging rows): detail-enrich
   *  every cinema, resolve ONCE over the whole film's merged view at its best
   *  year, stamp that resolution onto every row, then fold each year-variant
   *  separately. Returns true if the film concluded this pass. See the class
   *  comment for why resolution is per-film but folding is per-variant. */
  private def promoteFilm(filmRows: Seq[StagingRecord]): Boolean = {
    val norm = TitleNormalizer.sanitize(filmRows.head.title)
    logger.info(s"Staging: promoting '${filmRows.head.title}' (cinemas: ${filmRows.map(_.cinema.displayName).distinct.mkString(", ")})")

    // 1. Detail-enrich each cinema's row. `detailReady` is false when a cinema
    //    that DEFERS resolution couldn't get its detail yet (so TMDB would miss
    //    the director/originalTitle hints) — it waits for a later pass. A
    //    non-deferring detail cinema (and a non-detail cinema) is always ready.
    val detailReady = filmRows.forall { r =>
      enrichers.find(_.cinema == r.cinema) match {
        case Some(e) => val ok = fetchDetail(r, e); ok || !e.defersTmdbResolution
        case None    => true
      }
    }

    // 2. Re-read the film's rows (every year-variant) with detail merged in.
    val fresh = stagingRepository.findAll().filter(r => TitleNormalizer.sanitize(r.title) == norm)
    if (fresh.isEmpty || fresh.exists(_.record.tmdbConcluded) || !detailReady) return false

    // 3. Resolve ONCE over the WHOLE film's merged view at its best year — the
    //    LOWEST present cinema year (the release year most cinemas agree on; a
    //    stray cinema's production year is usually the higher outlier), else
    //    yearless. This mirrors `FilmCanonicalizer.clusterYear`'s present-year
    //    fallback, so the year the promoter resolves at is the year the corpus
    //    settles to; TMDB's ±1 tolerance still catches an off-by-one outlier.
    //    `_id`-sorted `findAll` keeps `fresh.head` deterministic.
    val resolveYear = fresh.flatMap(_.year).minOption
    val mergedHints = MovieRecordMerge.unionAll(fresh.map(_.record))
    resolveStaging(fresh.head.title, resolveYear, mergedHints) match {
      case None => false // transient TMDB failure — retry next pass
      case Some(resolved0) =>
        // 3b. Recover a missing IMDb cross-reference INLINE. A staging row never
        //     enters the cache, so the event-driven `ImdbIdResolver` (which reads
        //     + writes the cache) can't reach it; recover here so the row folds
        //     already carrying the id — the same end state the direct path's
        //     `ImdbIdMissing` chain produces. Search title mirrors
        //     `publishTmdbOutcome`: the TMDB original title, else the api-query'd
        //     clean title.
        val resolved =
          if (resolved0.tmdbId.isDefined && resolved0.imdbId.isEmpty)
            recoverImdbId(resolved0.originalTitle.getOrElse(MovieService.apiQuery(fresh.head.title)), resolveYear)
              .fold(resolved0)(id => resolved0.copy(imdbId = Some(id)))
          else resolved0
        // 4. Stamp the resolution onto EVERY cinema row of EVERY year-variant
        //    (preserving each row's own cinema slot) so each variant's fold cluster
        //    is uniformly resolved + carries that cinema's detail.
        val tmdbSlot = resolved.data.get(Tmdb)
        fresh.foreach { r =>
          val stamped = r.record.copy(
            tmdbId      = resolved.tmdbId,
            imdbId      = resolved.imdbId,
            tmdbNoMatch = resolved.tmdbNoMatch,
            data        = tmdbSlot.fold(r.record.data)(s => r.record.data + (Tmdb -> s)))
          stagingRepository.upsert(r.cinema, r.title, r.year, stamped)
        }
        val concluded = resolved.tmdbConcluded
        // 5. Fold each year-variant separately — one event per distinct year.
        if (concluded) fresh.map(_.year).distinct.foreach { y =>
          logger.info(s"Staging: '${fresh.head.title}' (${y.getOrElse("?")}) → concluded (tmdbId=${resolved.tmdbId.getOrElse("—")}), folding into movies")
          onConcluded(StagingRecord(fresh.head.cinema, fresh.head.title, y, resolved))
        }
        concluded
    }
  }

  /** Fetch + merge this cinema's detail into its staging row. Returns true once
   *  the detail page has been fetched successfully (or was already merged). The
   *  fetch is ALWAYS attempted (not skipped on listing-supplied poster/synopsis)
   *  so a cinema's detail-only cast isn't lost — `mergeInto` is idempotent, so a
   *  re-fetch only fills gaps. */
  private def fetchDetail(row: StagingRecord, enricher: DetailEnricher): Boolean = {
    val target = enricher.detailTarget
    row.record.data.get(row.cinema).flatMap(_.filmUrl).flatMap(enricher.fetchFilmDetail) match {
      case Some(detail) =>
        val merged = row.record.copy(
          data = row.record.data + (target -> detail.mergeInto(row.record.data.getOrElse(target, models.SourceData()))))
        stagingRepository.upsert(row.cinema, row.title, row.year, merged)
        logger.info(s"Staging: '${row.title}' ← detail from ${row.cinema.displayName}")
        true
      case None =>
        // No filmUrl, or the fetch failed: already-merged detail still counts as ready.
        row.record.data.get(target).exists(s => s.synopsis.isDefined || s.cast.nonEmpty || s.director.nonEmpty)
    }
  }
}

package services.movies

import models.{MovieRecord, Source}
import play.api.Logging
import services.staging.StagingRepository

import java.util.concurrent.{Executors, TimeUnit}
import scala.util.Try

/**
 * Sends the cinemas of a row's SECOND film back to staging, so each film ends up
 * with a record of its own.
 *
 * Two unrelated films released under one Polish title share a row, because the row
 * is keyed by title. `FilmCanonicalizer.clusterByFilm` would keep them apart, but
 * it splits ROWS by tmdbId and a row holding both films has only one — so whatever
 * it resolves to, the cinemas showing the other film are served the wrong record.
 * "Joanna d'Arc" holds Besson's 1999 film and Pálmason's 2025 one and resolves to
 * neither; "Obcy" serves Ozon's "L'étranger" to the one cinema screening Brandt
 * Andersen's film.
 *
 * Rather than build a second way to create a record, this undoes the merge: the
 * stray slots go back to `pending_movies`, exactly as a newcomer's scrape diverts
 * them, and the ordinary staging path takes it from there — each row resolves on
 * its OWN hints (`StagingSteps.hintGroupKey` already groups by director and
 * original title, so they resolve apart), and the fold gives each cluster its own
 * `movies` row. Dropping the slots here also means the next scrape tick sees the
 * cinema as untouched on the old row, so nothing is left pointing at both.
 *
 * Deliberately conservative — see `MixedFilmDetector`. Only a positive
 * contradiction between what two cinemas PUBLISHED counts, never one cinema
 * merely describing the film less fully than another, and never anything derived
 * from the resolution being checked.
 */
class MixedFilmSplitter(cache: MovieCache, staging: StagingRepository) extends Logging {

  /** Re-divert every stray slot in the corpus. Returns how many were sent back.
   *
   *  Idempotent: a split row no longer holds the slots that made it mixed, so a
   *  second pass over the same corpus finds nothing. Safe to run on a cadence. */
  def splitMixedRows(): Int = {
    val work = cache.snapshot().flatMap { entry =>
      val strays = MixedFilmDetector.strays(entry.record, cache.normalizer)
      if (strays.isEmpty) None else Some((entry, strays))
    }

    work.foldLeft(0) { case (moved, (entry, strays)) =>
      val key = cache.keyOf(entry.title, entry.year)
      strays.foreach { case (source, slot) =>
        Source.cinemaOf(source).foreach { cinema =>
          val title = slot.title.filter(_.trim.nonEmpty).getOrElse(entry.title)
          staging.upsert(cinema, title, slot.releaseYear, MovieRecord(
            searchTitle = Some(cache.normalizer.apiQuery(cache.normalizer.recase(title))),
            data        = Map(source -> slot)))
          logger.info(s"Mixed-film split: '${entry.title}' (${entry.year.getOrElse("?")}) — " +
            s"${cinema.displayName} screens a different film " +
            s"[director: ${slot.director.mkString(", ")}; original: ${slot.originalTitle.getOrElse("—")}] " +
            s"→ re-diverted to staging as '$title' (${slot.releaseYear.getOrElse("?")}).")
        }
      }
      // Drop them from the row in ONE update so the row is never observed holding
      // a slot staging has already taken over.
      val strayKeys = strays.map(_._1).toSet
      cache.putIfPresent(key, r => r.copy(data = r.data -- strayKeys))
      moved + strays.size
    }
  }

  // A mixed row only appears when a cinema starts screening a same-titled film, so
  // this is rare and never urgent — a slow cadence keeps it off the hot path. The
  // startup delay lets the cache rehydrate first, so the sweep sees the real corpus
  // rather than an empty one.
  private val scheduler = Executors.newSingleThreadScheduledExecutor(r => {
    val t = new Thread(r, "mixed-film-splitter"); t.setDaemon(true); t
  })
  private val StartupDelaySeconds = 300L
  private val RunEveryHours       = 6L

  def start(): Unit = {
    logger.info(s"Mixed-film split scheduled every ${RunEveryHours}h (first run in ${StartupDelaySeconds}s).")
    scheduler.scheduleAtFixedRate(
      () => Try(splitMixedRows()).recover {
        case exception => logger.warn(s"Mixed-film split tick failed: ${exception.getMessage}")
      },
      StartupDelaySeconds, RunEveryHours * 3600, TimeUnit.SECONDS
    )
  }

  def stop(): Unit = scheduler.shutdown()
}

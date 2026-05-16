package scripts

import clients.TmdbClient
import models.MovieRecord
import services.events.InProcessEventBus
import services.movies.{CaffeineMovieCache, MongoMovieRepo, MovieService}

/**
 * One-shot: invoke `MovieService.retryUnresolvedTmdb()` against real Mongo to
 * heal rows currently stuck at `tmdbId=None`. Required after landing the
 * fix to `retryUnresolvedTmdb` that passes cinemaShowings-derived director +
 * originalTitle hints — without this run, production rows that were trapped
 * by the old blind retry would have to wait up to 24h for the next
 * scheduled tick.
 *
 * Idempotent: rows whose TMDB still genuinely can't resolve them (even with
 * director hint + cinemaOriginalTitle) stay at `tmdbId=None` and the retry
 * marks them negative again. Rows that DO resolve get their tmdbId, imdbId
 * (when TMDB has a cross-reference), and originalTitle written back through
 * the cache → Mongo.
 *
 * Output (per CLAUDE.md "Scripts must print what they did"):
 *   - sample of resolved rows with their new tmdbId / imdbId / originalTitle
 *   - count of still-unresolved rows (TMDB genuinely can't match)
 *   - throughput summary
 *
 * Run with: sbt "Test/runMain scripts.TmdbRetryRescue"
 */
object TmdbRetryRescue {

  def main(args: Array[String]): Unit = {
    val repo = new MongoMovieRepo()
    if (!repo.enabled) { println("MONGODB_URI not set."); sys.exit(1) }

    val before: Map[String, MovieRecord] =
      repo.findAll().collect { case s if s.record.tmdbId.isEmpty => keyOf(s.title, s.year) -> s.record }.toMap
    println(s"${before.size} row(s) currently have tmdbId=None — running retryUnresolvedTmdb…\n")

    val cache = new CaffeineMovieCache(repo)
    val svc   = new MovieService(cache, new InProcessEventBus(), new TmdbClient())

    val startedAt = System.currentTimeMillis()
    svc.retryUnresolvedTmdb()
    svc.stop()                 // drains worker pool; in-flight writes have landed
    val elapsedSec = (System.currentTimeMillis() - startedAt) / 1000.0

    val after: Map[String, MovieRecord] =
      repo.findAll().map(s => keyOf(s.title, s.year) -> s.record).toMap
    repo.close()

    val resolved = before.toSeq.flatMap { case (k, b) =>
      after.get(k).filter(_.tmdbId.isDefined).map(a => (k, b, a))
    }.sortBy(_._1)
    val stillUnresolved = before.toSeq.flatMap { case (k, _) =>
      after.get(k).filter(_.tmdbId.isEmpty).map(_ => k)
    }.sorted

    val Sample = 20
    if (resolved.nonEmpty) {
      println(s"════ Resolved rows (first $Sample) ════")
      resolved.take(Sample).foreach { case (k, _, a) =>
        println(s"  $k")
        println(s"    → tmdbId=${a.tmdbId.getOrElse("—")}  imdbId=${a.imdbId.getOrElse("—")}  originalTitle=${a.originalTitle.getOrElse("—")}  director=${a.director.getOrElse("—")}")
      }
      if (resolved.size > Sample) println(s"  (+ ${resolved.size - Sample} more)")
    } else {
      println("No rows resolved on this pass.")
    }

    println()
    println("════ Summary ════")
    println(s"  Resolved        : ${resolved.size}")
    println(s"  Still unresolved: ${stillUnresolved.size}")
    println(f"  Done in $elapsedSec%.1fs (worker pool=10).")

    if (stillUnresolved.nonEmpty) {
      println()
      println(s"════ Still unresolved (TMDB genuinely can't match) — showing first $Sample ════")
      stillUnresolved.take(Sample).foreach(k => println(s"  $k"))
      if (stillUnresolved.size > Sample) println(s"  (+ ${stillUnresolved.size - Sample} more)")
    }
  }

  // Display-only join key. Matches the title spelling stored in Mongo so the
  // before/after JOIN is exact — not the normalised cache key (which would
  // hide which spelling was actually stored).
  private def keyOf(title: String, year: Option[Int]): String =
    s"$title (${year.map(_.toString).getOrElse("?")})"
}

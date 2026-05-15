package scripts

import clients.TmdbClient
import models.MovieRecord
import services.movies.MovieRepo

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, TimeUnit}
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

/**
 * Year-less re-resolution backfill: walks every Mongo row whose key still
 * carries a year and asks TMDB to resolve `(title, None)` — the same lookup
 * the post-MultikinoClient-fix flow performs (ShowtimeCache now publishes
 * `MovieAdded` with year=None for MK films; `tmdb.search` year-less ranking
 * prefers popular exact-title matches).
 *
 * Why this is needed: an earlier wave of rows was keyed by the cinema's
 * scheduling year (treated as the film year), so year-scoped TMDB search
 * picked a coincidentally-same-year film. Example: "Powrót do przyszłości"
 * (cinema reported 2026, then re-keyed to 2024 by `CacheKeyYearBackfill`)
 * resolves to Alienoid 2 (tt20201212) under year-scoped search but to Back
 * to the Future (tt0088763) under year-less ranking. The user has confirmed
 * the year-less answer is what the cinemas actually screen.
 *
 * Strategy per year-keyed row:
 *   - Call `tmdb.search(title, None)` and look up the IMDb id of the hit.
 *   - If the new `imdbId` equals the row's `imdbId`: leave the row alone
 *     (the year-scoped and year-less paths agree, no orphan).
 *   - If different: write a new row at `(title, None)` carrying only the
 *     new TMDB-side fields (imdbId, tmdbId, originalTitle). All score / URL
 *     fields are cleared — they belonged to a different film and will be
 *     refilled by the dedicated `*Ratings` classes on their next ticks.
 *     Delete the old row.
 *
 * Output follows CLAUDE.md: BEFORE → AFTER per row, sampled summary, throughput.
 *
 * Run: sbt "Test/runMain scripts.YearLessReResolve"
 */
object YearLessReResolve {
  private sealed trait Outcome
  private case class Replaced(title: String, before: (Option[Int], Option[String]), after: (Option[Int], Option[String])) extends Outcome
  private case class Agreed  (title: String, year: Option[Int], imdbId: Option[String])                                   extends Outcome
  private case class Skipped (title: String, year: Option[Int], reason: String)                                           extends Outcome

  def main(args: Array[String]): Unit = {
    val repo = new MovieRepo()
    if (!repo.enabled) { println("MONGODB_URI not set — nothing to backfill."); sys.exit(1) }
    val tmdb = new TmdbClient()

    val rows         = repo.findAll()
    val yearKeyed    = rows.filter { case (_, y, _) => y.isDefined }
    val noneKeyed    = rows.count   { case (_, y, _) => y.isEmpty   }
    println(s"${rows.size} rows in Mongo · $noneKeyed at year=None · ${yearKeyed.size} year-keyed (candidates for re-resolve)")
    val Workers = 5
    println(s"Probing TMDB year-less with $Workers workers in parallel…\n")

    val pool = Executors.newFixedThreadPool(Workers)
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)
    val done        = new AtomicInteger(0)
    val total       = yearKeyed.size
    val startedAtMs = System.currentTimeMillis()

    val tasks: Seq[Future[Outcome]] = yearKeyed.map { case (title, year, e) =>
      Future {
        val idx = done.incrementAndGet()
        val newHit   = Try(tmdb.search(title, None)).toOption.flatten
        val newImdb  = newHit.flatMap(h => Try(tmdb.imdbId(h.id)).toOption.flatten)

        (newHit, newImdb, e.imdbId) match {
          case (None, _, _) =>
            Skipped(title, year, "year-less search returned no hit")
          case (_, None, _) =>
            Skipped(title, year, "year-less search hit had no IMDb cross-reference")
          case (Some(hit), Some(newId), Some(oldId)) if newId == oldId =>
            // Year-scoped and year-less agree — row is correctly keyed
            // *enough*: same film, just tagged with a non-None year. Leave it.
            Agreed(title, year, e.imdbId)
          case (Some(hit), Some(newId), _) =>
            // Year-less picked a different (or new) imdbId. Treat as orphan
            // and re-write at year=None. Score fields cleared deliberately —
            // they belonged to a different film and will be refilled by the
            // ratings services on their next ticks (or by the next pass of
            // `EnrichmentBackfill`).
            val replacement = MovieRecord(
              imdbId            = Some(newId),
              imdbRating        = None,
              metascore         = None,
              originalTitle     = hit.originalTitle,
              filmwebUrl        = None,
              filmwebRating     = None,
              rottenTomatoes    = None,
              tmdbId            = Some(hit.id),
              metacriticUrl     = None,
              rottenTomatoesUrl = None
            )
            repo.upsert(title, None, replacement)
            repo.delete(title, year)
            val before = (year, e.imdbId)
            val after  = (None: Option[Int], Some(newId): Option[String])
            println(f"[$idx%3d/$total%3d] REPLACED $title")
            println(s"             was:  year=${year.getOrElse("None")}  imdbId=${e.imdbId.getOrElse("None")}  orig=${e.originalTitle.getOrElse("—")}")
            println(s"             now:  year=None  imdbId=$newId  orig=${hit.originalTitle.getOrElse("—")}")
            Replaced(title, before, after)
        }
      }
    }

    val outcomes = Await.result(Future.sequence(tasks), 30.minutes)
    pool.shutdown()
    pool.awaitTermination(30, TimeUnit.SECONDS)
    repo.close()

    val replaced = outcomes.collect { case r: Replaced => r }
    val agreed   = outcomes.collect { case a: Agreed   => a }
    val skipped  = outcomes.collect { case s: Skipped  => s }

    val elapsedSec = (System.currentTimeMillis() - startedAtMs) / 1000.0
    val rps        = if (elapsedSec > 0) f"${total / elapsedSec}%.1f" else "—"

    println()
    println(s"════ Summary ════")
    println(s"  Replaced : ${replaced.size}  (year-less search picked a different imdbId; re-keyed to None)")
    println(s"  Agreed   : ${agreed.size}    (year-scoped and year-less agree; row left as-is)")
    println(s"  Skipped  : ${skipped.size}   (no year-less hit / no IMDb cross-reference)")
    println(f"  Done in $elapsedSec%.1fs, ~$rps rows/s across $Workers workers.")
  }
}

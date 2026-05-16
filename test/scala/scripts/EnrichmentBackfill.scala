package scripts

import clients.TmdbClient
import models.MovieRecord
import services.enrichment.{FilmwebClient, FilmwebRatings, ImdbClient, ImdbRatings, MetacriticClient, MetascoreRatings, RottenTomatoesClient, RottenTomatoesRatings}
import services.movies.{CaffeineMovieCache, MongoMovieRepo, MovieService}
import services.events.EventBus

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
 * One-shot: walk every (title, year) currently in `kinowo.enrichments` and
 * re-run the full enrichment pipeline via `MovieService.reEnrichSync`,
 * so newly-added fields and TMDB index drift get picked up. Reads
 * MONGODB_URI / TMDB_API_KEY from `.env.local` via tools.Env.
 *
 * Output follows CLAUDE.md: BEFORE → AFTER for each row where TMDB resolves
 * to a different film (imdbId change), compact per-row line for refreshes,
 * and a sample-capped summary at the end.
 *
 * Run with: sbt "Test/runMain scripts.EnrichmentBackfill"
 */
object EnrichmentBackfill {
  private sealed trait Outcome
  private case class Changed(title: String, year: Option[Int], before: MovieRecord, after: MovieRecord) extends Outcome
  private case class Refreshed(title: String, year: Option[Int], after: MovieRecord) extends Outcome
  private case class Failed(title: String, year: Option[Int], before: MovieRecord) extends Outcome

  def main(args: Array[String]): Unit = {
    val repo = new MongoMovieRepo()
    if (!repo.enabled) {
      println("MONGODB_URI not set — nothing to backfill.")
      sys.exit(1)
    }

    // Bus isn't wired to anything in the script — reEnrichSync runs the TMDB
    // and IMDb stages synchronously. MC / RT URL discovery + score scrape
    // now live in their dedicated ratings classes; the script invokes them
    // directly per row so a single backfill pass covers everything.
    val cache       = new CaffeineMovieCache(repo)
    val tmdb        = new TmdbClient()
    val imdbRatings = new ImdbRatings(cache, new ImdbClient())
    val mcRatings   = new MetascoreRatings(cache, tmdb, new MetacriticClient())
    val rtRatings   = new RottenTomatoesRatings(cache, tmdb, new RottenTomatoesClient())
    val fwRatings   = new FilmwebRatings(cache, tmdb, new FilmwebClient())
    val service = new MovieService(cache, new EventBus(), tmdb)

    val rows = repo.findAll().sortBy { case (t, y, _) => (t.toLowerCase, y) }
    val Workers = 5
    println(s"${rows.size} rows in Mongo · re-enriching in parallel ($Workers workers)…\n")

    val pool = Executors.newFixedThreadPool(Workers)
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)
    val done           = new AtomicInteger(0)
    val refreshedShown = new AtomicInteger(0)
    val total          = rows.size
    val RefreshedSampleSize = 10
    val startedAtMs    = System.currentTimeMillis()

    val tasks = rows.map { case (title, year, before) =>
      Future {
        service.reEnrichSync(title, year)
        // Drive the per-row work for the ratings classes directly so a single
        // pass over Mongo covers TMDB + IMDb + MC + RT + Filmweb. Each
        // *Ratings.refreshOneSync handles its own URL discovery (where needed)
        // and score scrape; the order doesn't matter (they're independent).
        imdbRatings.refreshOneSync(title, year)
        mcRatings.refreshOneSync(title, year)
        rtRatings.refreshOneSync(title, year)
        fwRatings.refreshOneSync(title, year)
        val after = service.get(title, year)
        val idx   = done.incrementAndGet()
        val outcome: Outcome = after match {
          case Some(e) if e.imdbId != before.imdbId =>
            Changed(title, year, before, e)
          case Some(e) =>
            Refreshed(title, year, e)
          case None =>
            Failed(title, year, before)
        }
        outcome match {
          case Changed(t, y, b, a) =>
            // The interesting case: TMDB now resolves the (title, year) to a
            // different film than the previous enrichment. Show BEFORE → AFTER
            // so the user can verify the new pick is correct.
            println(f"[$idx%3d/$total%3d] CHANGED $t (${y.getOrElse("?")})")
            println(s"             was:  imdbId=${b.imdbId}  orig=${b.originalTitle.getOrElse("—")}")
            println(s"             now:  imdbId=${a.imdbId}  orig=${a.originalTitle.getOrElse("—")}" +
                    s"  imdb=${a.imdbRating.map(r => f"$r%.1f").getOrElse("—")}" +
                    s"  mc=${if (a.metacriticUrl.isDefined) "✓" else "—"}" +
                    s"  rt=${if (a.rottenTomatoesUrl.isDefined) "✓" else "—"}")
          case Refreshed(t, y, a) =>
            // imdbId unchanged — just refreshed the ratings/URLs. Print the
            // first N for spot-check; the rest get summarised.
            if (refreshedShown.incrementAndGet() <= RefreshedSampleSize) {
              println(f"[$idx%3d/$total%3d] refreshed $t (${y.getOrElse("?")}) " +
                      s"imdb=${a.imdbRating.map(r => f"$r%.1f").getOrElse("—")} " +
                      s"fw=${a.filmwebRating.map(r => f"$r%.1f").getOrElse("—")} " +
                      s"mc=${if (a.metacriticUrl.isDefined) "✓" else "—"} " +
                      s"rt=${if (a.rottenTomatoesUrl.isDefined) "✓" else "—"}")
            }
          case Failed(t, y, b) =>
            println(f"[$idx%3d/$total%3d] FAILED  $t (${y.getOrElse("?")}) — TMDB no longer resolves" +
                    s" (was imdbId=${b.imdbId})")
        }
        outcome
      }
    }

    val outcomes = Await.result(Future.sequence(tasks), 30.minutes)
    pool.shutdown()
    service.stop()
    repo.close()

    val changed   = outcomes.collect { case c: Changed   => c }
    val refreshed = outcomes.collect { case r: Refreshed => r }
    val failed    = outcomes.collect { case f: Failed    => f }

    if (refreshed.size > RefreshedSampleSize)
      println(s"  (+ ${refreshed.size - RefreshedSampleSize} more refreshed rows — same imdbId, ratings/URLs updated)")

    val elapsedSec = (System.currentTimeMillis() - startedAtMs) / 1000.0
    val rps        = if (elapsedSec > 0) f"${total / elapsedSec}%.1f" else "—"

    println()
    println(s"════ Summary ════")
    println(s"  Changed (imdbId shift): ${changed.size}")
    println(s"  Refreshed (same imdbId): ${refreshed.size}")
    println(s"  Failed   (TMDB no match): ${failed.size}")
    println(f"  Done in $elapsedSec%.1fs, ~$rps rows/s across $Workers workers.")

    if (changed.nonEmpty) {
      println()
      println(s"════ Review the ${changed.size} changed rows ════")
      changed.foreach { case Changed(t, y, b, a) =>
        println(s"  $t (${y.getOrElse("?")}):  ${b.imdbId} → ${a.imdbId}  [${b.originalTitle.getOrElse("—")} → ${a.originalTitle.getOrElse("—")}]")
      }
    }
  }
}

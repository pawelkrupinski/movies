package scripts

import clients.TmdbClient
import models.Enrichment
import services.enrichment._

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
 * One-shot: walk every (title, year) currently in `kinowo.enrichments` and
 * re-run the full enrichment pipeline via `EnrichmentService.reEnrichSync`,
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
  private case class Changed(title: String, year: Option[Int], before: Enrichment, after: Enrichment) extends Outcome
  private case class Refreshed(title: String, year: Option[Int], after: Enrichment) extends Outcome
  private case class Failed(title: String, year: Option[Int], before: Enrichment) extends Outcome

  def main(args: Array[String]): Unit = {
    val repo = new EnrichmentRepo()
    if (!repo.enabled) {
      println("MONGODB_URI not set — nothing to backfill.")
      sys.exit(1)
    }

    val service = new EnrichmentService(
      new EnrichmentCache(repo),
      new TmdbClient(), new FilmwebClient(), new ImdbClient(),
      new MetacriticClient(), new RottenTomatoesClient()
    )

    val rows = repo.findAll().sortBy { case (t, y, _) => (t.toLowerCase, y) }
    val Workers = 5
    println(s"${rows.size} rows in Mongo · re-enriching in parallel ($Workers workers)…\n")

    val pool = Executors.newFixedThreadPool(Workers)
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)
    val done    = new AtomicInteger(0)
    val refreshedShown = new AtomicInteger(0)
    val total   = rows.size
    val RefreshedSampleSize = 10

    val tasks = rows.map { case (title, year, before) =>
      Future {
        val after = service.reEnrichSync(title, year)
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

    println()
    println(s"════ Summary ════")
    println(s"  Changed (imdbId shift): ${changed.size}")
    println(s"  Refreshed (same imdbId): ${refreshed.size}")
    println(s"  Failed   (TMDB no match): ${failed.size}")

    if (changed.nonEmpty) {
      println()
      println(s"════ Review the ${changed.size} changed rows ════")
      changed.foreach { case Changed(t, y, b, a) =>
        println(s"  $t (${y.getOrElse("?")}):  ${b.imdbId} → ${a.imdbId}  [${b.originalTitle.getOrElse("—")} → ${a.originalTitle.getOrElse("—")}]")
      }
    }
  }
}

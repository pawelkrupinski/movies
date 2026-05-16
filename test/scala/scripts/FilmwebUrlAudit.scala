package scripts

import clients.TmdbClient
import services.enrichment.{FilmwebClient, FilmwebRatings}
import services.movies.{MovieCache, MovieRepo}

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
 * Audit + backfill: walk every row in Mongo that has a `filmwebUrl` and
 * re-run the tightened `FilmwebClient.lookup` (canonical-title bar + optional
 * TMDB director check) via `FilmwebRatings.auditOneSync`. The helper replaces
 * the row's URL when the new lookup picks a different canonical id, drops
 * the URL when no candidate clears the bar, otherwise leaves the row alone.
 *
 * Why this is needed: the previous `lookup` had no title acceptance bar — it
 * just picked the year-closest of the top search hits, even when none of
 * them had anything to do with the film. The DB therefore holds Filmweb URLs
 * that point to entirely unrelated films, most visibly on rows where
 * Filmweb is the only available rating link (every other matcher already
 * rejected the same fuzzy hit). CLAUDE.md mandates a backfill whenever
 * ingestion / maintenance logic changes that would produce a different
 * stored value, which is exactly the case here.
 *
 * Output (per CLAUDE.md "Scripts must print what they did"):
 *   - per-row BEFORE → AFTER for changed rows
 *   - sample (10) of dropped + corrected URLs at the end
 *   - throughput summary
 *
 * Run with: sbt "Test/runMain scripts.FilmwebUrlAudit"
 */
object FilmwebUrlAudit {

  def main(args: Array[String]): Unit = {
    val repo = new MovieRepo()
    if (!repo.enabled) {
      println("MONGODB_URI not set — nothing to audit.")
      sys.exit(1)
    }
    val cache   = new MovieCache(repo)
    val tmdb    = new TmdbClient()
    val filmweb = new FilmwebClient()
    val ratings = new FilmwebRatings(cache, tmdb, filmweb)

    val candidates = repo.findAll()
      .filter { case (_, _, e) => e.filmwebUrl.isDefined }
      .sortBy { case (t, y, _) => (t.toLowerCase, y) }
    val Workers = 3  // CLAUDE.md: Filmweb soft-blocks above ~5.
    println(s"${candidates.size} row(s) carry a filmwebUrl — auditing with $Workers workers.\n")

    val pool = Executors.newFixedThreadPool(Workers)
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)
    val done       = new AtomicInteger(0)
    val total      = candidates.size
    val startedAt  = System.currentTimeMillis()

    val tasks = candidates.map { case (title, year, _) =>
      Future {
        val outcome = ratings.auditOneSync(title, year)
        val idx = done.incrementAndGet()
        outcome match {
          case FilmwebRatings.Corrected(before, after) =>
            println(f"[$idx%4d/$total%4d] CORRECTED $title (${yearLabel(year)})")
            println(s"             before: $before")
            println(s"             after : $after")
          case FilmwebRatings.Dropped(before) =>
            println(f"[$idx%4d/$total%4d] DROPPED   $title (${yearLabel(year)})")
            println(s"             was: $before")
          case FilmwebRatings.Kept(_) | FilmwebRatings.NoUrl => () // silent; counted in summary.
        }
        (title, year, outcome)
      }
    }

    val outcomes = Await.result(Future.sequence(tasks), 60.minutes)
    pool.shutdown()
    repo.close()

    val kept      = outcomes.collect { case (_, _, _: FilmwebRatings.Kept)      => () }
    val corrected = outcomes.collect { case (t, y, c: FilmwebRatings.Corrected) => (t, y, c) }
    val dropped   = outcomes.collect { case (t, y, d: FilmwebRatings.Dropped)   => (t, y, d) }
    val noUrl     = outcomes.collect { case (_, _, FilmwebRatings.NoUrl)        => () }

    val elapsedSec = (System.currentTimeMillis() - startedAt) / 1000.0
    val rps        = if (elapsedSec > 0) f"${total / elapsedSec}%.1f" else "—"

    println()
    println(s"════ Summary ════")
    println(s"  Kept (canonical URL still resolves to the same film): ${kept.size}")
    println(s"  Corrected (URL replaced with the right canonical):    ${corrected.size}")
    println(s"  Dropped   (no candidate clears the matching bar):     ${dropped.size}")
    println(s"  NoUrl     (row gone between snapshot and audit):      ${noUrl.size}")
    println(f"  Done in $elapsedSec%.1fs, ~$rps rows/s across $Workers workers.")

    val Sample = 10
    if (dropped.nonEmpty) {
      println()
      println(s"════ Dropped rows (first $Sample) ════")
      dropped.take(Sample).foreach { case (t, y, d) =>
        println(s"  $t (${yearLabel(y)}) ← ${d.before}")
      }
      if (dropped.size > Sample) println(s"  (+ ${dropped.size - Sample} more)")
    }
    if (corrected.nonEmpty) {
      println()
      println(s"════ Corrected rows (first $Sample) ════")
      corrected.take(Sample).foreach { case (t, y, c) =>
        println(s"  $t (${yearLabel(y)})")
        println(s"    ${c.before}")
        println(s"    → ${c.after}")
      }
      if (corrected.size > Sample) println(s"  (+ ${corrected.size - Sample} more)")
    }
  }

  private def yearLabel(y: Option[Int]): String = y.map(_.toString).getOrElse("?")
}

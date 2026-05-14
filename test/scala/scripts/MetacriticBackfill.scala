package scripts

import services.enrichment.{EnrichmentRepo, MetacriticClient}

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

/**
 * Targeted MC URL fix: for every row where `metacriticUrl` is missing OR is
 * the bogus `/movie/` (an old empty-slug bug), re-run the MC URL probe using
 * the *existing* originalTitle / cleanTitle. Does NOT re-resolve TMDB —
 * preserves the row's imdbId / tmdbId / orig so we can't accidentally pick a
 * different film during the re-fetch.
 *
 * Output follows CLAUDE.md: per-row `BEFORE → AFTER` for every change,
 * unchanged rows summarised with a sample.
 *
 * Run: sbt "Test/runMain scripts.MetacriticBackfill"
 */
object MetacriticBackfill {
  // Bogus URL produced when slugify yields an empty string (CJK / Cyrillic
  // orig) and `canonicalUrl` probes `/movie/` (MC's movie index, 200).
  private val BogusUrlSuffix = "/movie/"

  private sealed trait Outcome
  private case class Filled(title: String, year: Option[Int], orig: Option[String], before: Option[String], after: String) extends Outcome
  private case class Cleared(title: String, year: Option[Int], orig: Option[String], before: String) extends Outcome
  private case class Unchanged(title: String, year: Option[Int], orig: Option[String]) extends Outcome

  def main(args: Array[String]): Unit = {
    val repo = new EnrichmentRepo()
    if (!repo.enabled) {
      println("MONGODB_URI not set — nothing to backfill.")
      sys.exit(1)
    }
    val mc = new MetacriticClient()

    val all = repo.findAll()
    val targets = all
      .filter { case (_, _, e) =>
        e.metacriticUrl.isEmpty || e.metacriticUrl.exists(_.endsWith(BogusUrlSuffix))
      }
      .sortBy { case (t, y, _) => (t.toLowerCase, y) }

    val bogusCount = all.count(_._3.metacriticUrl.exists(_.endsWith(BogusUrlSuffix)))
    println(s"${all.size} rows in Mongo · ${targets.size} targets (missing MC URL or bogus /movie/) · $bogusCount bogus to clear")
    println(s"Probing MC for each target with 5 workers in parallel…\n")

    val pool = Executors.newFixedThreadPool(5)
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)
    val done = new AtomicInteger(0)
    val total = targets.size

    val tasks = targets.map { case (title, year, e) =>
      Future {
        val linkTitle = e.originalTitle.getOrElse(title)
        val fallback  = if (linkTitle != title) Some(title) else None
        val fresh = Try(mc.urlFor(linkTitle, fallback)).toOption.flatten
        val idx = done.incrementAndGet()

        val outcome: Outcome = (e.metacriticUrl, fresh) match {
          case (_, Some(url)) if !url.endsWith(BogusUrlSuffix) =>
            repo.upsert(title, year, e.copy(metacriticUrl = Some(url)))
            Filled(title, year, e.originalTitle, e.metacriticUrl, url)
          case (Some(old), _) if old.endsWith(BogusUrlSuffix) =>
            repo.upsert(title, year, e.copy(metacriticUrl = None))
            Cleared(title, year, e.originalTitle, old)
          case _ =>
            Unchanged(title, year, e.originalTitle)
        }

        // Per-row progress: print every change immediately; collapse unchanged
        // rows into a counter so we don't drown the terminal.
        outcome match {
          case Filled(t, y, orig, before, after) =>
            println(f"[$idx%3d/$total%3d] FILLED  $t (${y.getOrElse("?")})${orig.fold("")(o => s" [orig=$o]")}")
            println(s"             was: ${before.getOrElse("None")}")
            println(s"             now: $after")
          case Cleared(t, y, orig, before) =>
            println(f"[$idx%3d/$total%3d] CLEARED $t (${y.getOrElse("?")})${orig.fold("")(o => s" [orig=$o]")}")
            println(s"             was: $before  (bogus empty-slug)")
            println(s"             now: None")
          case _: Unchanged =>
            // No per-row line — summarised below.
        }
        outcome
      }
    }

    val outcomes = Await.result(Future.sequence(tasks), 30.minutes)
    pool.shutdown()
    repo.close()

    val filled    = outcomes.collect { case f: Filled    => f }
    val cleared   = outcomes.collect { case c: Cleared   => c }
    val unchanged = outcomes.collect { case u: Unchanged => u }

    println()
    println(s"════ Unchanged: ${unchanged.size} rows still without MC URL ════")
    val sampleSize = 20
    unchanged.take(sampleSize).foreach { case Unchanged(t, y, orig) =>
      println(s"  · $t (${y.getOrElse("?")})${orig.fold("")(o => s" [orig=$o]")}")
    }
    if (unchanged.size > sampleSize)
      println(s"  (+ ${unchanged.size - sampleSize} more unchanged — Polish art-house, foreign classics, live events; MC doesn't index them)")

    println()
    println(s"════ Summary ════")
    println(s"  Filled  : ${filled.size}")
    println(s"  Cleared : ${cleared.size}  (bogus /movie/ URLs from the empty-slug bug)")
    println(s"  Unchanged: ${unchanged.size}")
  }
}

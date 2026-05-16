package scripts

import services.enrichment.MetacriticClient
import services.movies.{MongoMovieRepo, StoredMovieRecord}

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

/**
 * MC URL revalidation: walk every row in Mongo and re-run `metacritic.urlFor`
 * on the row's stored `originalTitle` (with `cleanTitle` as fallback). Writes
 * the new result whenever it differs from the stored URL. Picks up:
 *
 *   - **Filled**:    None              → new canonical URL (just-added fixes)
 *   - **Corrected**: wrong canonical   → different canonical or None
 *                                        (e.g. cleanup of bad rows from a
 *                                        previous over-eager partial-match
 *                                        backfill)
 *   - **Cleared**:   bogus `/movie/`   → None (historic empty-slug bug)
 *
 * Does NOT re-resolve TMDB — preserves the row's imdbId / tmdbId / orig so
 * we can't accidentally pick a different film. Output follows CLAUDE.md:
 * per-row BEFORE → AFTER for every change; unchanged rows summarised.
 *
 * Run: sbt "Test/runMain scripts.MetacriticBackfill"
 */
object MetacriticBackfill {
  private val BogusUrlSuffix = "/movie/"

  private sealed trait Outcome
  private case class Filled   (title: String, year: Option[Int], orig: Option[String], after: String)                  extends Outcome
  private case class Corrected(title: String, year: Option[Int], orig: Option[String], before: String, after: String)  extends Outcome
  private case class Cleared  (title: String, year: Option[Int], orig: Option[String], before: String)                 extends Outcome
  private case class Unchanged(title: String, year: Option[Int], orig: Option[String])                                 extends Outcome

  def main(args: Array[String]): Unit = {
    val repo = new MongoMovieRepo()
    if (!repo.enabled) {
      println("MONGODB_URI not set — nothing to backfill.")
      sys.exit(1)
    }
    val mc = new MetacriticClient()

    val rows = repo.findAll().sortBy(r => (r.title.toLowerCase, r.year))
    val bogusCount = rows.count(_.record.metacriticUrl.exists(_.endsWith(BogusUrlSuffix)))
    // 10 workers is the upper end of CLAUDE.md's "5–10" range for
    // undocumented services. MC handles concurrent probes well in practice
    // — `urlFor` issues at most 2-3 GETs per row (slug probe + de-articled
    // probe + optional search scrape), so 10 workers means ~20-30 in-flight
    // requests against MC at peak. Halve to 5 if we ever see 429/503 back.
    val Workers = 10
    println(s"${rows.size} rows in Mongo · revalidating every MC URL · " +
            s"${rows.count(_.record.metacriticUrl.isEmpty)} currently None · $bogusCount currently bogus /movie/")
    println(s"Probing MC with $Workers workers in parallel…\n")

    val pool = Executors.newFixedThreadPool(Workers)
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)
    val started     = new AtomicInteger(0)
    val done        = new AtomicInteger(0)
    val total       = rows.size
    val startedAtMs = System.currentTimeMillis()

    val tasks = rows.map { case StoredMovieRecord(title, year, e) =>
      Future {
        // Per-row trace so the user can see progress and which row is in
        // flight when something stalls (a slow MC search-scrape, a 503
        // backoff, a flaky DNS lookup). The matching DONE line below pairs
        // with this so the worker activity is auditable.
        val sIdx = started.incrementAndGet()
        println(f"[$sIdx%3d/$total%3d] STARTED   $title (${year.getOrElse("?")})")

        val linkTitle = e.originalTitle.getOrElse(title)
        val fallback  = if (linkTitle != title) Some(title) else None
        // Pass the row's year so MC's search-scrape fallback can disambiguate
        // by release year (e.g. "Annie (2014)" vs older "Annie" entries).
        val fresh = Try(mc.urlFor(linkTitle, fallback, year)).toOption.flatten
        val idx = done.incrementAndGet()

        val outcome: Outcome = (e.metacriticUrl, fresh) match {
          // Bogus stored URL: always replace (clear or correct).
          case (Some(old), Some(url)) if old.endsWith(BogusUrlSuffix) && !url.endsWith(BogusUrlSuffix) =>
            repo.upsert(title, year, e.copy(metacriticUrl = Some(url)))
            Corrected(title, year, e.originalTitle, old, url)
          case (Some(old), None) if old.endsWith(BogusUrlSuffix) =>
            repo.upsert(title, year, e.copy(metacriticUrl = None))
            Cleared(title, year, e.originalTitle, old)
          // Stored URL is None → potential fill.
          case (None, Some(url)) if !url.endsWith(BogusUrlSuffix) =>
            repo.upsert(title, year, e.copy(metacriticUrl = Some(url)))
            Filled(title, year, e.originalTitle, url)
          // Stored URL exists and differs from new probe result → correct it.
          case (Some(old), Some(url)) if old != url =>
            repo.upsert(title, year, e.copy(metacriticUrl = Some(url)))
            Corrected(title, year, e.originalTitle, old, url)
          case (Some(old), None) =>
            repo.upsert(title, year, e.copy(metacriticUrl = None))
            Corrected(title, year, e.originalTitle, old, "None")
          // Same URL (or both None) — no write needed.
          case _ =>
            Unchanged(title, year, e.originalTitle)
        }

        outcome match {
          case Filled(t, y, orig, after) =>
            println(f"[$idx%3d/$total%3d] FILLED    $t (${y.getOrElse("?")})${orig.fold("")(o => s" [orig=$o]")}")
            println(s"             was: None")
            println(s"             now: $after")
          case Corrected(t, y, orig, before, after) =>
            println(f"[$idx%3d/$total%3d] CORRECTED $t (${y.getOrElse("?")})${orig.fold("")(o => s" [orig=$o]")}")
            println(s"             was: $before")
            println(s"             now: $after")
          case Cleared(t, y, orig, before) =>
            println(f"[$idx%3d/$total%3d] CLEARED   $t (${y.getOrElse("?")})${orig.fold("")(o => s" [orig=$o]")}")
            println(s"             was: $before  (bogus empty-slug)")
            println(s"             now: None")
          case _: Unchanged => ()  // summarised below
        }
        outcome
      }
    }

    val outcomes = Await.result(Future.sequence(tasks), 30.minutes)
    pool.shutdown()
    repo.close()

    val filled    = outcomes.collect { case f: Filled    => f }
    val corrected = outcomes.collect { case c: Corrected => c }
    val cleared   = outcomes.collect { case c: Cleared   => c }
    val unchanged = outcomes.collect { case u: Unchanged => u }

    // Split unchanged into "had a URL all along" vs "still None" — the latter
    // is the legit-not-on-MC set worth sampling.
    val unchangedStillNone = unchanged.filter { case Unchanged(t, y, _) =>
      // We don't carry MC URL on Unchanged, so look it up via the closure:
      // pull from `rows` for this (title, year) key.
      rows.find(r => r.title == t && r.year == y).flatMap(_.record.metacriticUrl).isEmpty
    }

    println()
    println(s"════ Unchanged-None: ${unchangedStillNone.size} rows that have no MC URL and aren't fixable ════")
    val sampleSize = 20
    unchangedStillNone.take(sampleSize).foreach { case Unchanged(t, y, orig) =>
      println(s"  · $t (${y.getOrElse("?")})${orig.fold("")(o => s" [orig=$o]")}")
    }
    if (unchangedStillNone.size > sampleSize)
      println(s"  (+ ${unchangedStillNone.size - sampleSize} more — Polish art-house, foreign classics, live events; MC doesn't index them)")

    val elapsedMs  = System.currentTimeMillis() - startedAtMs
    val elapsedSec = elapsedMs / 1000.0
    val rps        = if (elapsedSec > 0) f"${total / elapsedSec}%.1f" else "—"

    println()
    println(s"════ Summary ════")
    println(s"  Filled    : ${filled.size}    (None → canonical URL)")
    println(s"  Corrected : ${corrected.size}  (wrong URL → different URL or None)")
    println(s"  Cleared   : ${cleared.size}   (bogus /movie/ → None)")
    println(s"  Unchanged : ${unchanged.size}  (${unchangedStillNone.size} still None, ${unchanged.size - unchangedStillNone.size} confirmed correct)")
    println(f"  Done in ${elapsedSec}%.1fs, ~$rps rows/s across $Workers workers.")
  }
}

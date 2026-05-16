package scripts

import services.enrichment.RottenTomatoesClient
import services.movies.{MongoMovieRepo, StoredMovieRecord}

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

/**
 * RT URL + score revalidation: walk every row in Mongo and re-run
 * `rt.urlFor` on the row's stored `originalTitle` (with `cleanTitle` as
 * fallback) and `rt.scoreFor` against the resolved URL. Writes the new
 * values whenever they differ from what's stored.
 *
 * Required because two ingestion-time behaviours just changed and the new
 * results don't reach existing rows until they're re-enriched:
 *   1. `rt.urlFor` now accepts (fallback, year) and falls through to a
 *      search-page scrape — fills rows that previously got None.
 *   2. `rt.scoreFor` is a brand-new field-populating call — every row's
 *      `rottenTomatoes` is currently None (the OmdbClient that used to
 *      populate it was removed).
 *
 * Does NOT re-resolve TMDB — preserves the row's imdbId / tmdbId / orig so
 * we can't accidentally pick a different film. Output follows CLAUDE.md:
 * per-row BEFORE → AFTER for every change; unchanged rows summarised.
 *
 * Run: sbt "Test/runMain scripts.RottenTomatoesBackfill"
 */
object RottenTomatoesBackfill {
  private val BogusUrlSuffix = "/m/"

  private sealed trait Change
  private case object NoChange       extends Change
  private case class  UrlFilled    (after: String)                 extends Change
  private case class  UrlCorrected (before: String, after: String) extends Change
  private case class  UrlCleared   (before: String)                extends Change
  private case class  ScoreFilled  (after: Int)                    extends Change
  private case class  ScoreCorrected(before: Int, after: Int)      extends Change
  private case class  ScoreCleared (before: Int)                   extends Change

  def main(args: Array[String]): Unit = {
    val repo = new MongoMovieRepo()
    if (!repo.enabled) {
      println("MONGODB_URI not set — nothing to backfill.")
      sys.exit(1)
    }
    val rt = new RottenTomatoesClient()

    val rows = repo.findAll().sortBy(r => (r.title.toLowerCase, r.year))
    val bogusUrlCount = rows.count(_.record.rottenTomatoesUrl.exists(_.endsWith(BogusUrlSuffix)))
    val missingScore = rows.count(_.record.rottenTomatoes.isEmpty)
    // 10 workers per CLAUDE.md ("5–10 for undocumented services"). Each row
    // makes at most 3 GETs: slug probe → optional search scrape → score page.
    // Halve if RT starts returning 429/503.
    val Workers = 10
    println(s"${rows.size} rows in Mongo · revalidating RT URL + score · " +
            s"${rows.count(_.record.rottenTomatoesUrl.isEmpty)} URL None · $bogusUrlCount URL bogus /m/ · " +
            s"$missingScore score None")
    println(s"Probing RT with $Workers workers in parallel…\n")

    val pool = Executors.newFixedThreadPool(Workers)
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)
    val started = new AtomicInteger(0)
    val done    = new AtomicInteger(0)
    val total   = rows.size
    val t0      = System.currentTimeMillis()

    val tasks = rows.map { case StoredMovieRecord(title, year, e) =>
      Future {
        val sIdx = started.incrementAndGet()
        println(f"[$sIdx%3d/$total%3d] STARTED   $title (${year.getOrElse("?")})")

        val linkTitle = e.originalTitle.getOrElse(title)
        val fallback  = if (linkTitle != title) Some(title) else None
        val freshUrl  = Try(rt.urlFor(linkTitle, fallback, year)).toOption.flatten
        // Only score-probe when we landed on a real /m/ page — empty-slug or
        // search-result URLs would scrape nothing useful.
        val freshScore = freshUrl
          .filterNot(_.endsWith(BogusUrlSuffix))
          .flatMap(u => Try(rt.scoreFor(u)).toOption.flatten)

        val urlChange: Change = (e.rottenTomatoesUrl, freshUrl) match {
          case (Some(old), Some(u)) if old.endsWith(BogusUrlSuffix) && !u.endsWith(BogusUrlSuffix) =>
            UrlCorrected(old, u)
          case (Some(old), None) if old.endsWith(BogusUrlSuffix) =>
            UrlCleared(old)
          case (None, Some(u)) if !u.endsWith(BogusUrlSuffix) =>
            UrlFilled(u)
          case (Some(old), Some(u)) if old != u =>
            UrlCorrected(old, u)
          case (Some(old), None) =>
            UrlCorrected(old, "None")
          case _ => NoChange
        }

        val scoreChange: Change = (e.rottenTomatoes, freshScore) match {
          case (None, Some(s))               => ScoreFilled(s)
          case (Some(old), Some(s)) if old != s => ScoreCorrected(old, s)
          case (Some(old), None)             => ScoreCleared(old)
          case _                              => NoChange
        }

        if (urlChange != NoChange || scoreChange != NoChange) {
          repo.upsert(title, year, e.copy(
            rottenTomatoesUrl = freshUrl,
            rottenTomatoes    = freshScore
          ))
        }

        val idx = done.incrementAndGet()
        val orig = e.originalTitle.fold("")(o => s" [orig=$o]")
        urlChange match {
          case UrlFilled(after)             => println(f"[$idx%3d/$total%3d] URL.FILLED    $title (${year.getOrElse("?")})$orig\n             was: None\n             now: $after")
          case UrlCorrected(before, after) => println(f"[$idx%3d/$total%3d] URL.CORRECTED $title (${year.getOrElse("?")})$orig\n             was: $before\n             now: $after")
          case UrlCleared(before)          => println(f"[$idx%3d/$total%3d] URL.CLEARED   $title (${year.getOrElse("?")})$orig\n             was: $before  (bogus empty-slug)\n             now: None")
          case _                            => ()
        }
        scoreChange match {
          case ScoreFilled(after)             => println(f"[$idx%3d/$total%3d] RT.FILLED     $title (${year.getOrElse("?")})$orig  None → $after%%")
          case ScoreCorrected(before, after) => println(f"[$idx%3d/$total%3d] RT.CORRECTED  $title (${year.getOrElse("?")})$orig  $before%% → $after%%")
          case ScoreCleared(before)          => println(f"[$idx%3d/$total%3d] RT.CLEARED    $title (${year.getOrElse("?")})$orig  $before%% → None")
          case _                              => ()
        }
        (urlChange, scoreChange)
      }
    }

    val outcomes = Await.result(Future.sequence(tasks), 30.minutes)
    pool.shutdown()
    repo.close()

    val urlChanges   = outcomes.count { case (u, _) => u != NoChange }
    val scoreChanges = outcomes.count { case (_, s) => s != NoChange }
    val urlFilled    = outcomes.count { case (_: UrlFilled,    _) => true; case _ => false }
    val urlCorrected = outcomes.count { case (_: UrlCorrected, _) => true; case _ => false }
    val urlCleared   = outcomes.count { case (_: UrlCleared,   _) => true; case _ => false }
    val scoreFilled  = outcomes.count { case (_, _: ScoreFilled)    => true; case _ => false }
    val scoreFixed   = outcomes.count { case (_, _: ScoreCorrected) => true; case _ => false }
    val scoreClearedN = outcomes.count { case (_, _: ScoreCleared)   => true; case _ => false }

    val elapsedSec = (System.currentTimeMillis() - t0) / 1000.0
    val rps        = if (elapsedSec > 0) f"${total / elapsedSec}%.1f" else "—"

    println()
    println(s"════ Summary ════")
    println(s"  URL   : filled $urlFilled · corrected $urlCorrected · cleared $urlCleared · unchanged ${total - urlChanges}")
    println(s"  Score : filled $scoreFilled · corrected $scoreFixed · cleared $scoreClearedN · unchanged ${total - scoreChanges}")
    println(f"  Done in ${elapsedSec}%.1fs, ~$rps rows/s across $Workers workers.")
  }
}

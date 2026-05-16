package scripts

import clients.TmdbClient
import services.enrichment.{FilmwebClient, FilmwebRatings}
import services.events.InProcessEventBus
import services.movies.{CaffeineMovieCache, MongoMovieRepo, MovieService, StoredMovieRecord}

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

/**
 * One-shot: wipe `filmwebUrl` + `filmwebRating` from every row in Mongo, then
 * re-resolve via the current (tightened) `FilmwebClient.lookup`. The view
 * layer's FW badge is gated on `filmwebRating.isDefined` — any row whose
 * fresh lookup can't clear the title + director bar ends up with both fields
 * `None` and is silently dropped from the UI; no stale or wrong-film URLs
 * survive.
 *
 * Output (per row): `URL_BEFORE  RATING_BEFORE → URL_AFTER  RATING_AFTER`,
 * plus a categorised summary at the end. Parallelised at 5 workers — Filmweb
 * soft-blocks anything higher (CLAUDE.md).
 *
 * Run: sbt "Test/runMain scripts.FilmwebReset"
 */
object FilmwebReset {

  private sealed trait Outcome
  private case object Cleared                                   extends Outcome  // had URL, no fresh match → both None
  private case object NoChangeAlreadyClear                      extends Outcome  // had None, still None
  private case class  FilledFresh(url: String, rating: Option[Double])    extends Outcome
  private case class  ReplacedSame(url: String, rating: Option[Double])   extends Outcome  // same URL, possibly fresh rating
  private case class  ReplacedDifferent(beforeUrl: String, afterUrl: String, rating: Option[Double]) extends Outcome

  def main(args: Array[String]): Unit = {
    val repo = new MongoMovieRepo()
    if (!repo.enabled) {
      println("MONGODB_URI not set — nothing to reset.")
      sys.exit(1)
    }

    val tmdb    = new TmdbClient()
    val filmweb = new FilmwebClient()

    // ── Phase 1: snapshot, then wipe FW fields on every row ────────────────
    //
    // We capture (title, year, before-state) up front so the post-reset
    // re-resolution can compare. The wipe writes through to Mongo and the
    // cache so a subsequent FilmwebRatings.refreshOneSync starts from a
    // clean slate (filmwebUrl=None forces the full discovery path, not the
    // cheap rating-only refresh).
    val before: Seq[StoredMovieRecord] = repo.findAll().sortBy(r => (r.title.toLowerCase, r.year))
    println(s"${before.size} rows in Mongo · clearing filmwebUrl + filmwebRating on all of them…")

    before.foreach { r =>
      val cleared = r.record.copy(filmwebUrl = None, filmwebRating = None)
      repo.upsert(r.title, r.year, cleared)
    }
    println("Clear pass done.")

    // ── Phase 2: hydrate a fresh cache + ratings + re-resolve every row ────
    //
    // Building CaffeineMovieCache(repo) AFTER the wipe means hydration sees
    // the cleared values; refreshOneSync's filmwebUrl=None branch runs the
    // full lookup (search → /info → optional /preview → /rating).
    val cache   = new CaffeineMovieCache(repo)
    val ratings = new FilmwebRatings(cache, tmdb, filmweb)
    // `MovieService.get` is the only public window into the cache from a
    // script — used post-refresh to read back the newly-resolved values.
    val svc     = new MovieService(cache, new InProcessEventBus(), tmdb)

    val Workers = 5  // CLAUDE.md: Filmweb soft-blocks above ~5.
    val pool    = Executors.newFixedThreadPool(Workers)
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)
    val done       = new AtomicInteger(0)
    val total      = before.size
    val startedAt  = System.currentTimeMillis()

    println(s"Re-resolving each row with $Workers workers…\n")

    val tasks: Seq[Future[(String, Option[Int], Outcome)]] = before.map { r =>
      Future {
        ratings.refreshOneSync(r.title, r.year)

        // Read the post-refresh state from the cache (cheap, in-memory).
        // FilmwebRatings.refreshOneSync write-throughs to both cache and
        // Mongo, so this reflects the fresh resolution.
        val after: Option[(String, Option[Double])] =
          svc.get(r.title, r.year).flatMap(e => e.filmwebUrl.map(u => u -> e.filmwebRating))
        val idx = done.incrementAndGet()

        val outcome: Outcome = (r.record.filmwebUrl, after) match {
          case (Some(beforeUrl), Some((afterUrl, afterRating))) if beforeUrl == afterUrl =>
            ReplacedSame(afterUrl, afterRating)
          case (Some(beforeUrl), Some((afterUrl, afterRating))) =>
            ReplacedDifferent(beforeUrl, afterUrl, afterRating)
          case (None, Some((afterUrl, afterRating))) =>
            FilledFresh(afterUrl, afterRating)
          case (Some(_), None) =>
            Cleared
          case (None, None) =>
            NoChangeAlreadyClear
        }

        val ratingStr = (rating: Option[Double]) => rating.map(d => f"$d%.1f").getOrElse("—")
        val beforeStr = s"${r.record.filmwebUrl.getOrElse("—")}  ${ratingStr(r.record.filmwebRating)}"
        outcome match {
          case ReplacedSame(_, ar) =>
            println(f"[$idx%4d/$total%4d] KEPT      ${r.title} (${yearLabel(r.year)})  ${ratingStr(r.record.filmwebRating)} → ${ratingStr(ar)}")
          case ReplacedDifferent(b, a, ar) =>
            println(f"[$idx%4d/$total%4d] CHANGED   ${r.title} (${yearLabel(r.year)})")
            println(s"             before: $b")
            println(s"             after : $a  rating=${ratingStr(ar)}")
          case FilledFresh(a, ar) =>
            println(f"[$idx%4d/$total%4d] FILLED    ${r.title} (${yearLabel(r.year)})  → $a  rating=${ratingStr(ar)}")
          case Cleared =>
            println(f"[$idx%4d/$total%4d] CLEARED   ${r.title} (${yearLabel(r.year)})  was: $beforeStr")
          case NoChangeAlreadyClear =>
            ()  // counted in summary; not interesting per-row.
        }
        (r.title, r.year, outcome)
      }
    }

    val outcomes = Await.result(Future.sequence(tasks), 60.minutes)
    pool.shutdown()
    repo.close()

    val kept       = outcomes.count(_._3.isInstanceOf[ReplacedSame])
    val changed    = outcomes.count(_._3.isInstanceOf[ReplacedDifferent])
    val filled     = outcomes.count(_._3.isInstanceOf[FilledFresh])
    val cleared    = outcomes.count(_._3 == Cleared)
    val alreadyNone = outcomes.count(_._3 == NoChangeAlreadyClear)

    val elapsedSec = (System.currentTimeMillis() - startedAt) / 1000.0
    val rps        = if (elapsedSec > 0) f"${total / elapsedSec}%.1f" else "—"

    println()
    println(s"════ Summary ════")
    println(s"  Kept     (re-resolved to same canonical URL):              $kept")
    println(s"  Changed  (re-resolved to a DIFFERENT URL):                 $changed")
    println(s"  Filled   (was None, fresh lookup found one):               $filled")
    println(s"  Cleared  (had URL before, no fresh match clears the bar): $cleared")
    println(s"  Already None (no URL before, still no URL):                $alreadyNone")
    println(f"  Done in $elapsedSec%.1fs, ~$rps rows/s across $Workers workers.")
  }

  private def yearLabel(y: Option[Int]): String = y.map(_.toString).getOrElse("?")

  // Silence unused-import warnings on Try (kept for parity with sibling scripts).
  Try(())
}

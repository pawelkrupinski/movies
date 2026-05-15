package scripts

import clients.TmdbClient
import services.movies.MovieRepo

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, TimeUnit}
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

/**
 * Backfill for the MultikinoClient year fix: walk every Mongo row, ask TMDB
 * what year the row's `imdbId` is actually from, and re-key the row when its
 * stored year disagrees. Rows are stored keyed by `(normalize(title), year)`,
 * so a "wrong year" means the Mongo `_id` is misfiled and future lookups by
 * `(title, cinemaYear)` won't find the row.
 *
 * The contamination this fixes: Multikino's `releaseDate` field is the Polish
 * theatrical (re-)release date — e.g. "Zawieście czerwone latarnie" (Raise
 * the Red Lantern, 1991) comes back as 2026-06-18. The parser used to store
 * year=2026; the row's `_id` then was `"zawiescie czerwone latarnie|2026"`
 * instead of `"zawiescie czerwone latarnie|1991"`.
 *
 * Strategy per row:
 *   - If `imdbId` is None: skip (we have no anchor to compare against).
 *   - If TMDB lookup for `imdbId` returns the same `releaseYear` as the
 *     stored row: row is correctly keyed → leave it alone.
 *   - Otherwise: the row is misfiled. If a row at the correct `(title,
 *     trueYear)` *also* exists, merge into whichever has more populated
 *     fields and delete the other. If not, re-key the row (delete old, write
 *     new) preserving all its data.
 *
 * Output follows CLAUDE.md: per-row BEFORE → AFTER for every move, plus
 * throughput.
 *
 * Run: sbt "Test/runMain scripts.CacheKeyYearBackfill"
 */
object CacheKeyYearBackfill {
  private sealed trait Outcome
  private case class Rekeyed (title: String, before: Option[Int], after: Option[Int], imdbId: String) extends Outcome
  private case class Merged  (title: String, drop:   Option[Int], kept:  Option[Int], imdbId: String) extends Outcome
  private case class Unchanged(title: String, year: Option[Int], reason: String)                     extends Outcome

  def main(args: Array[String]): Unit = {
    val repo = new MovieRepo()
    if (!repo.enabled) { println("MONGODB_URI not set — nothing to backfill."); sys.exit(1) }
    val tmdb = new TmdbClient()

    val rows = repo.findAll()
    println(s"${rows.size} rows in Mongo · checking each against TMDB's release year")

    // Group rows by normalised title so we can detect "row at correct key
    // already exists" before re-keying — without a check we'd clobber data.
    val byTitle = rows.groupBy { case (t, _, _) =>
      services.movies.MovieService.normalize(t)
    }
    val total   = rows.size
    val Workers = 5
    println(s"Probing TMDB with $Workers workers in parallel…\n")

    val pool = Executors.newFixedThreadPool(Workers)
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)
    val done        = new AtomicInteger(0)
    val startedAtMs = System.currentTimeMillis()

    val tasks: Seq[Future[Outcome]] = rows.map { case (title, year, e) =>
      Future {
        val idx = done.incrementAndGet()
        // Pick the anchor TMDB lookup. imdbId is preferred (it cross-checks
        // against TMDB's `/find` mapping); fall back to tmdbId so rows TMDB
        // resolved without an IMDb cross-reference (very recent Polish
        // indies — e.g. "Chłopiec na krańcach świata", tmdbId=1277047,
        // imdbId=None) still get re-keyed by their TMDB release year.
        val tmdbYear: Option[Int] = e.imdbId match {
          case Some(id) => Try(tmdb.findByImdbId(id)).toOption.flatten.flatMap(_.releaseYear)
          case None     => e.tmdbId.flatMap(id => Try(tmdb.details(id)).toOption.flatten.flatMap(_.releaseYear))
        }
        val anchor: Option[String] = e.imdbId.orElse(e.tmdbId.map(id => s"tmdb:$id"))
        (tmdbYear, anchor) match {
          case (_, None) =>
            Unchanged(title, year, "no imdbId or tmdbId — can't compare")
          case (None, _) =>
            Unchanged(title, year, "TMDB lookup returned no release year")
          case (Some(_), Some(id)) =>
            if (tmdbYear == year) {
              Unchanged(title, year, "matches TMDB")
            } else {
              // Wrong key. Look for a sibling at (title, tmdbYear) in our
              // groupBy snapshot — that's the destination.
              val normalised = services.movies.MovieService.normalize(title)
              val sibling = byTitle.getOrElse(normalised, Seq.empty)
                .find { case (_, y, _) => y == tmdbYear && y != year }
              sibling match {
                case Some((sibT, sibY, sibE)) =>
                  // Merge: prefer the row with more populated fields.
                  val rowScore = populatedCount(e)
                  val sibScore = populatedCount(sibE)
                  if (rowScore > sibScore) {
                    // Current row is the keeper but it's misfiled — overwrite
                    // sibling at the correct key, drop the current.
                    repo.upsert(sibT, tmdbYear, e)
                    repo.delete(title, year)
                    println(f"[$idx%3d/$total%3d] MERGED   $title ($year→$tmdbYear)  imdbId=$id  (kept the misfiled row's richer data, replaced sibling)")
                    Merged(title, year, tmdbYear, id)
                  } else {
                    // Sibling is already richer; just delete the misfiled row.
                    repo.delete(title, year)
                    println(f"[$idx%3d/$total%3d] DROPPED  $title ($year)  imdbId=$id  (sibling at year=${tmdbYear.getOrElse("None")} already has more data)")
                    Merged(title, year, tmdbYear, id)
                  }
                case None =>
                  // No sibling — straight re-key.
                  repo.upsert(title, tmdbYear, e)
                  repo.delete(title, year)
                  println(f"[$idx%3d/$total%3d] REKEYED  $title  year=${year.getOrElse("None")} → year=${tmdbYear.getOrElse("None")}  imdbId=$id")
                  Rekeyed(title, year, tmdbYear, id)
              }
            }
        }
      }
    }

    val outcomes = Await.result(Future.sequence(tasks), 30.minutes)
    pool.shutdown()
    pool.awaitTermination(30, TimeUnit.SECONDS)
    repo.close()

    val rekeyed   = outcomes.collect { case r: Rekeyed   => r }
    val merged    = outcomes.collect { case m: Merged    => m }
    val unchanged = outcomes.collect { case u: Unchanged => u }

    val elapsedSec = (System.currentTimeMillis() - startedAtMs) / 1000.0
    val rps        = if (elapsedSec > 0) f"${total / elapsedSec}%.1f" else "—"

    println()
    println(s"════ Summary ════")
    println(s"  Rekeyed   : ${rekeyed.size}    (year moved without colliding)")
    println(s"  Merged    : ${merged.size}     (row at correct key existed; consolidated)")
    println(s"  Unchanged : ${unchanged.size}  (row's year already matched TMDB, or no imdbId to check)")
    println(f"  Done in $elapsedSec%.1fs, ~$rps rows/s across $Workers workers.")
  }

  private def populatedCount(e: models.MovieRecord): Int =
    Seq(e.imdbId, e.imdbRating, e.metascore, e.originalTitle, e.filmwebUrl,
        e.filmwebRating, e.tmdbId, e.metacriticUrl, e.rottenTomatoesUrl)
      .count(_.isDefined)
}

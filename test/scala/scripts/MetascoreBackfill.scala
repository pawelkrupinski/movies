package scripts

import services.enrichment.MetacriticClient
import services.movies.{MongoMovieRepo, StoredMovieRecord}

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, TimeUnit}
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

/**
 * Metascore backfill: walk every row that has a `metacriticUrl` but no
 * `metascore` (or whose stored score may have drifted), scrape the MC page,
 * and update Mongo. Mirrors `MetacriticBackfill` but writes the score field
 * instead of the URL.
 *
 * Output follows CLAUDE.md: per-row BEFORE → AFTER for every change, plus
 * a sampled list of unchanged-None rows at the end so it's clear which films
 * MC genuinely has no aggregate score for.
 *
 * Run: sbt "Test/runMain scripts.MetascoreBackfill"
 */
object MetascoreBackfill {
  private sealed trait Outcome
  private case class Filled   (title: String, year: Option[Int], orig: Option[String], after: Int)              extends Outcome
  private case class Corrected(title: String, year: Option[Int], orig: Option[String], before: Int, after: Int) extends Outcome
  private case class Unchanged(title: String, year: Option[Int], orig: Option[String], hasUrl: Boolean, current: Option[Int]) extends Outcome

  def main(args: Array[String]): Unit = {
    val repo = new MongoMovieRepo()
    if (!repo.enabled) {
      println("MONGODB_URI not set — nothing to backfill.")
      sys.exit(1)
    }
    val mc = new MetacriticClient()

    val rows = repo.findAll().sortBy(r => (r.title.toLowerCase, r.year))
    val withUrl = rows.count(_.record.metacriticUrl.isDefined)
    val withScore = rows.count(_.record.metascore.isDefined)
    println(s"${rows.size} rows in Mongo · $withUrl have MC URL · $withScore have metascore · revalidating every row's metascore")
    val Workers = 3
    println(s"Probing MC with $Workers workers in parallel…\n")

    val pool = Executors.newFixedThreadPool(Workers)
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(pool)
    val done        = new AtomicInteger(0)
    val total       = rows.size
    val startedAtMs = System.currentTimeMillis()

    val tasks = rows.map { case StoredMovieRecord(title, year, e) =>
      Future {
        val freshScore = e.metacriticUrl.flatMap(url => Try(mc.metascoreFor(url)).toOption.flatten)
        val idx = done.incrementAndGet()

        val outcome: Outcome = (e.metascore, freshScore) match {
          case (None, Some(s)) =>
            repo.upsert(title, year, e.copy(metascore = Some(s)))
            Filled(title, year, e.originalTitle, s)
          case (Some(old), Some(s)) if old != s =>
            repo.upsert(title, year, e.copy(metascore = Some(s)))
            Corrected(title, year, e.originalTitle, old, s)
          case _ =>
            Unchanged(title, year, e.originalTitle, e.metacriticUrl.isDefined, e.metascore)
        }

        outcome match {
          case Filled(t, y, orig, after) =>
            println(f"[$idx%3d/$total%3d] FILLED    $t (${y.getOrElse("?")})${orig.fold("")(o => s" [orig=$o]")}  None → $after")
          case Corrected(t, y, orig, before, after) =>
            println(f"[$idx%3d/$total%3d] CORRECTED $t (${y.getOrElse("?")})${orig.fold("")(o => s" [orig=$o]")}  $before → $after")
          case _: Unchanged => ()  // sampled below
        }
        outcome
      }
    }

    val outcomes = Await.result(Future.sequence(tasks), 60.minutes)
    pool.shutdown()
    pool.awaitTermination(30, TimeUnit.SECONDS)
    repo.close()

    val filled    = outcomes.collect { case f: Filled    => f }
    val corrected = outcomes.collect { case c: Corrected => c }
    val unchanged = outcomes.collect { case u: Unchanged => u }

    // Spotlight unchanged-and-still-None rows that have an MC URL (legit
    // "MC has the page but no aggregate score yet") vs ones without an MC
    // URL at all (legit "MC doesn't index this film").
    val stillNoneWithUrl    = unchanged.filter(u => u.hasUrl && u.current.isEmpty)
    val confirmedScore      = unchanged.count(_.current.isDefined)
    val sampleSize          = 15

    println()
    println(s"════ Rows with MC URL but no aggregate score (${stillNoneWithUrl.size}) ════")
    stillNoneWithUrl.take(sampleSize).foreach { u =>
      println(s"  · ${u.title} (${u.year.getOrElse("?")})${u.orig.fold("")(o => s" [orig=$o]")}")
    }
    if (stillNoneWithUrl.size > sampleSize)
      println(s"  (+ ${stillNoneWithUrl.size - sampleSize} more — MC has the page but hasn't aggregated reviews yet)")

    val elapsedSec = (System.currentTimeMillis() - startedAtMs) / 1000.0
    val rps        = if (elapsedSec > 0) f"${total / elapsedSec}%.1f" else "—"

    println()
    println(s"════ Summary ════")
    println(s"  Filled    : ${filled.size}    (None → score)")
    println(s"  Corrected : ${corrected.size}  (stored score → different score)")
    println(s"  Unchanged : ${unchanged.size}  ($confirmedScore confirmed, ${stillNoneWithUrl.size} still None with URL, ${unchanged.size - confirmedScore - stillNoneWithUrl.size} no URL at all)")
    println(f"  Done in $elapsedSec%.1fs, ~$rps rows/s across $Workers workers.")
  }
}

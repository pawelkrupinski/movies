package scripts

import org.mongodb.scala.MongoClient
import services.movies.{CaffeineMovieCache, MongoMovieRepository, MongoScreeningsRepository, MongoSlotsRepository, UnscreenedCleanup}
import tools.Env
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * One-shot backfill: the new `UnscreenedCleanup` ticks every 24h, but the
 * first tick on a fresh deploy would leave the existing pre-deploy cruft in
 * Mongo for up to a day. Run this once after rolling out the cleanup so the
 * accumulated "film dropped from every cinema" rows are pruned immediately.
 *
 * Output (per CLAUDE.md): list of rows about to be dropped (with their
 * tmdbId/imdbId so a glance at the sample confirms they're the genuine
 * one-off events / dropped films, not the active catalogue), plus
 * BEFORE/AFTER row counts.
 *
 * Run with: sbt "Test/runMain scripts.UnscreenedRowsBackfill"
 */
object UnscreenedRowsBackfill {

  def main(args: Array[String]): Unit = {
    val uri    = Env.get("MONGODB_URI").getOrElse { println("MONGODB_URI not set."); sys.exit(1) }
    val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")
    val client = MongoClient(uri)
    val db     = client.getDatabase(dbName)
    // The cleanup corroborates every candidate against the durable RECORD before deleting
    // it, and under the storage split a film's cinemas live in `movie_slots`, not in the
    // `movies` document. A repository wired without them reads every migrated film as
    // cinema-less — i.e. the script would reproduce the very bug the guard exists to stop.
    val repository = new MongoMovieRepository(Some(db), fallbackToOwnInit = false,
      screenings = Some(new MongoScreeningsRepository(Some(db))),
      slots      = Some(new MongoSlotsRepository(Some(db))), normalizer = titleNormalizer)
    require(repository.enabled, s"movies repository not enabled for $dbName")

    val before    = repository.findAll()
    val orphans   = before.filter(_.record.cinemaData.isEmpty)
    println(s"${before.size} row(s) in Mongo; ${orphans.size} have no current screenings — dropping…\n")

    val Sample = 20
    orphans.take(Sample).foreach { s =>
      println(s"  DROP  ${s.title} (${s.year.getOrElse("?")})  tmdbId=${s.record.tmdbId.getOrElse("—")}  imdbId=${s.record.imdbId.getOrElse("—")}")
    }
    if (orphans.size > Sample) println(s"  (+ ${orphans.size - Sample} more)")

    val cache   = new CaffeineMovieCache(repository, normalizer = titleNormalizer)
    val cleanup = new UnscreenedCleanup(cache, repository)
    val removed = cleanup.removeUnscreened()
    repository.close()
    client.close()

    println()
    println("════ Summary ════")
    println(s"  Rows deleted: $removed")
    println(s"  Rows remaining: ${before.size - removed}")
  }
}

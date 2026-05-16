package scripts

import services.movies.{CaffeineMovieCache, MongoMovieRepo, UnscreenedCleanup}

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
    val repo = new MongoMovieRepo()
    if (!repo.enabled) { println("MONGODB_URI not set."); sys.exit(1) }

    val before    = repo.findAll()
    val orphans   = before.filter(_.record.cinemaShowings.isEmpty)
    println(s"${before.size} row(s) in Mongo; ${orphans.size} have no current screenings — dropping…\n")

    val Sample = 20
    orphans.take(Sample).foreach { s =>
      println(s"  DROP  ${s.title} (${s.year.getOrElse("?")})  tmdbId=${s.record.tmdbId.getOrElse("—")}  imdbId=${s.record.imdbId.getOrElse("—")}")
    }
    if (orphans.size > Sample) println(s"  (+ ${orphans.size - Sample} more)")

    val cache   = new CaffeineMovieCache(repo)
    val cleanup = new UnscreenedCleanup(cache)
    val removed = cleanup.removeUnscreened()
    repo.close()

    println()
    println("════ Summary ════")
    println(s"  Rows deleted: $removed")
    println(s"  Rows remaining: ${before.size - removed}")
  }
}

package scripts

import services.movies.MongoMovieRepo

/**
 * One-shot backfill: drop Mongo rows whose stored title starts with
 * "DZIEŃ DZIECKA W APOLLO - ". `KinoApolloClient.cleanTitle` now strips that
 * banner prefix so subsequent scrapes write the base title (e.g. "Drzewo
 * Magii") via `recordCinemaScrape`'s redirect. The pre-fix rows are orphaned
 * — no parser will ever update them again. CLAUDE.md mandates a backfill
 * when parsing changes the value a fresh scrape would produce; deleting the
 * orphan is the simplest possible fix here (the next 5-minute Apollo scrape
 * tick lands the merged showtimes on the base row).
 *
 * Output (per CLAUDE.md):
 *   - the rows it found + what it would do
 *   - count summary
 *
 * IMPORTANT: run AFTER the parser fix is deployed, otherwise the running
 * server's next scrape tick would re-create the prefixed row.
 *
 * Run with: sbt "Test/runMain scripts.ApolloPrefixBackfill"
 */
object ApolloPrefixBackfill {

  private val Prefix = "DZIEŃ DZIECKA W APOLLO - "

  def main(args: Array[String]): Unit = {
    val repo = new MongoMovieRepo()
    if (!repo.enabled) { println("MONGODB_URI not set."); sys.exit(1) }

    val orphans = repo.findAll().filter(_.title.startsWith(Prefix))
    println(s"${orphans.size} Mongo row(s) with the '$Prefix' prefix — deleting…\n")

    orphans.foreach { s =>
      println(s"  DELETE  ${s.title} (${s.year.getOrElse("?")})")
      println(s"    tmdbId=${s.record.tmdbId.getOrElse("—")}  imdbId=${s.record.imdbId.getOrElse("—")}  cinemas=${s.record.cinemaShowings.keys.map(_.displayName).mkString(", ")}")
      repo.delete(s.title, s.year)
    }
    repo.close()

    println()
    println("════ Summary ════")
    println(s"  Rows deleted: ${orphans.size}")
    if (orphans.nonEmpty)
      println(s"  Next Apollo scrape tick (≤5 min) will land the merged showtimes on the base row(s).")
  }
}

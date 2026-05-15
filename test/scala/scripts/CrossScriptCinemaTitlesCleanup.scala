package scripts

import services.enrichment.{MovieCache, MovieRepo}
import controllers.TitleNormalizer

/**
 * One-shot: for every row in Mongo, drop `cinemaTitles` entries that don't
 * share a script with the row's `cleanTitle`. The merger now refuses
 * cross-script collapses and `recordCinemaScrape`'s redirect requires
 * matching normalisation, so cross-script entries can't be accumulated by
 * the current code — but legacy rows still carry leftovers (Polish row
 * with a stray `"ДИЯВОЛ НОСИТЬ ПРАДА 2"` in its `cinemaTitles`, for
 * example), which made `displayTitle` pick the Cyrillic spelling on `/`.
 *
 * Idempotent: re-running on a clean Mongo finds no cross-script entries
 * and reports zero changes.
 *
 * Run: sbt "Test/runMain scripts.CrossScriptCinemaTitlesCleanup"
 */
object CrossScriptCinemaTitlesCleanup {
  def main(args: Array[String]): Unit = {
    val repo = new MovieRepo()
    if (!repo.enabled) { println("MONGODB_URI not set."); sys.exit(1) }

    val rows = repo.findAll()
    println(s"@@ ${rows.size} rows read")

    var changed       = 0
    var sampleShown   = 0
    val SampleSize    = 15
    val startedAt     = System.currentTimeMillis()

    rows.foreach { case (title, year, e) =>
      val sameScript = e.cinemaTitles.filter(t => TitleNormalizer.sameScript(t, title))
      val crossDropped = e.cinemaTitles -- sameScript
      if (crossDropped.nonEmpty) {
        changed += 1
        if (sampleShown < SampleSize) {
          sampleShown += 1
          println(s"@@ '$title' (${year.getOrElse("?")})  dropped: $crossDropped  kept: $sameScript")
        }
        repo.upsert(title, year, e.copy(cinemaTitles = sameScript))
      }
    }
    if (changed > SampleSize) println(s"@@   (+ ${changed - SampleSize} more rows updated — same pattern)")

    val elapsedSec = (System.currentTimeMillis() - startedAt) / 1000.0
    println()
    println("════ Summary ════")
    println(s"  rows scanned:                  ${rows.size}")
    println(s"  rows with cross-script titles: $changed")
    println(f"  done in $elapsedSec%.1fs")

    repo.close()
  }
}

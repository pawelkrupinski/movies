package scripts

import services.enrichment.MovieRepo

/**
 * Phase-1 backfill for the MovieCache transition.
 *
 * Pre-existing rows in Mongo don't have a `cinemaTitles` field — the decoder
 * defaults missing values to `Set.empty`. This walks every row and re-writes
 * it, which causes the encoder to materialise `cinemaTitles = {cleanTitle}`
 * for any row that's still empty. Idempotent: rows whose set already contains
 * `cleanTitle` come out unchanged.
 *
 * No semantic change to anything that reads enrichment data today. Phase 2
 * will use `cinemaTitles` as the corpus anchor for the new merge-key docId.
 *
 * Run: sbt "Test/runMain scripts.CinemaTitlesBackfill"
 */
object CinemaTitlesBackfill {
  def main(args: Array[String]): Unit = {
    val repo = new MovieRepo()
    if (!repo.enabled) { println("MONGODB_URI not set — nothing to backfill."); sys.exit(1) }

    val rows = repo.findAll()
    println(s"@@ ${rows.size} rows read from Mongo")

    var alreadyPopulated = 0
    var willPopulate     = 0
    var sampleShown      = 0
    val SampleSize       = 10
    val startedAt        = System.currentTimeMillis()

    rows.foreach { case (title, year, e) =>
      val before = e.cinemaTitles
      // Touch the row — encode/decode is idempotent if cinemaTitles already
      // contains cleanTitle, but we ALSO want repo.upsert(title, year, e) to
      // run so the new `cinemaTitles` array lands in the BSON for rows that
      // were stored before phase 1.
      val merged = e.copy(cinemaTitles = before + title)
      repo.upsert(title, year, merged)
      val after = merged.cinemaTitles

      if (before.contains(title)) alreadyPopulated += 1
      else {
        willPopulate += 1
        if (sampleShown < SampleSize) {
          sampleShown += 1
          println(s"@@ filled  '$title' (${year.getOrElse("?")})  before=$before  →  after=$after")
        }
      }
    }
    if (willPopulate > SampleSize)
      println(s"@@   (+ ${willPopulate - SampleSize} more rows populated — same pattern)")

    val elapsedSec = (System.currentTimeMillis() - startedAt) / 1000.0
    println()
    println("════ Summary ════")
    println(s"  rows touched:        ${rows.size}")
    println(s"  already populated:   $alreadyPopulated  (re-written unchanged)")
    println(s"  newly populated:     $willPopulate")
    println(f"  done in $elapsedSec%.1fs")

    repo.close()
  }
}

package scripts

import models.CinemaScrape
import services.movies.{MongoMovieRepo, StoredMovieRecord}

/**
 * One-shot backfill: rewrite all-uppercase titles stored in Mongo to sentence
 * case, matching the new `MultikinoParser.normaliseCase` behaviour. CLAUDE.md
 * mandates a backfill whenever parsing changes the value a fresh scrape
 * would produce.
 *
 * Touches two places per row:
 *   - The row's top-level `title` field (used by the controller as the
 *     display anchor when there's nothing better to fall back to).
 *   - Every `cinemaScrapes` entry whose `title` is all-uppercase (the
 *     provenance set the cache uses to decide which (cinema, title, year)
 *     tuples it has already seen).
 *
 * Title-case-only rows are left untouched. Output (per CLAUDE.md):
 *   - BEFORE → AFTER per row touched (with which fields changed)
 *   - count summary
 *
 * IMPORTANT: run this AFTER the `MultikinoParser.normaliseCase` fix is
 * deployed. The running production server's 5-minute scrape tick keeps
 * writing the cinema's verbatim string; a pre-deploy backfill would race
 * the next tick and lose. Post-deploy the new parser ships sentence-case,
 * so the backfill's writes stick and stay stuck.
 *
 * Run with: sbt "Test/runMain scripts.AllCapsTitleBackfill"
 */
object AllCapsTitleBackfill {

  def main(args: Array[String]): Unit = {
    val repo = new MongoMovieRepo()
    if (!repo.enabled) { println("MONGODB_URI not set."); sys.exit(1) }

    val rows = repo.findAll()
    println(s"${rows.size} row(s) in Mongo — scanning for all-uppercase titles…\n")

    val startedAt = System.currentTimeMillis()
    val touched = rows.flatMap(maybeRewrite)

    touched.foreach { case Change(before, after, changedTitle, changedScrapes) =>
      val parts = (if (changedTitle) Seq("title") else Seq.empty) ++
                  (if (changedScrapes.nonEmpty) Seq(s"${changedScrapes.size} cinemaScrape(s)") else Seq.empty)
      println(s"  ${before.title} (${before.year.getOrElse("?")})   [${parts.mkString(", ")}]")
      if (changedTitle) println(s"    title:    ${before.title}  →  ${after.title}")
      changedScrapes.foreach { case (oldT, newT) =>
        println(s"    scrape:   $oldT  →  $newT")
      }
    }

    touched.foreach { case Change(_, after, _, _) =>
      repo.upsert(after.title, after.year, after.record)
    }
    val elapsedSec = (System.currentTimeMillis() - startedAt) / 1000.0
    repo.close()

    println()
    println("════ Summary ════")
    println(s"  Rows rewritten: ${touched.size}")
    println(f"  Done in $elapsedSec%.1fs.")
  }

  private case class Change(
    before:         StoredMovieRecord,
    after:          StoredMovieRecord,
    changedTitle:   Boolean,
    changedScrapes: Seq[(String, String)]
  )

  private def maybeRewrite(row: StoredMovieRecord): Option[Change] = {
    val newTitle    = sentenceCase(row.title)
    val titleChanged = newTitle != row.title

    val scrapeChanges = row.record.cinemaScrapes.toSeq.flatMap { s =>
      val nt = sentenceCase(s.title)
      if (nt != s.title) Some((s, s.copy(title = nt))) else None
    }
    val scrapeRenameMap: Map[CinemaScrape, CinemaScrape] = scrapeChanges.toMap

    if (!titleChanged && scrapeChanges.isEmpty) None
    else {
      val newScrapes = row.record.cinemaScrapes.map(s => scrapeRenameMap.getOrElse(s, s))
      val newRecord  = row.record.copy(cinemaScrapes = newScrapes)
      val after      = StoredMovieRecord(newTitle, row.year, newRecord)
      Some(Change(
        before         = row,
        after          = after,
        changedTitle   = titleChanged,
        changedScrapes = scrapeChanges.map { case (b, a) => b.title -> a.title }
      ))
    }
  }

  // Same rule as MultikinoParser.normaliseCase: if any character is lowercase,
  // leave the title byte-identical; otherwise capitalise the first letter and
  // lowercase the rest, preserving any non-letter prefix ("90. ").
  private def sentenceCase(title: String): String =
    if (title.exists(_.isLower)) title
    else {
      val idx = title.indexWhere(_.isLetter)
      if (idx < 0) title
      else title.substring(0, idx) + title.charAt(idx).toUpper + title.substring(idx + 1).toLowerCase
    }
}

package scripts

import controllers.TitleNormalizer
import services.enrichment.MovieRepo

/**
 * Phase-1 audit for the upcoming `MovieCache` transition.
 *
 * Question this answers: if we re-key every stored enrichment row by
 * `TitleNormalizer.mergeKeyLookup` (today's display-time merge rule, applied
 * with the stored `cleanTitle`s as corpus) plus `year`, which rows would
 * collapse into a single record, and do any of those collapses look wrong?
 *
 * Output:
 *   1. Every multi-row group, with each row's cleanTitle / year / imdbId /
 *      originalTitle / tmdbId, sorted by group size descending.
 *   2. For each group, a SUSPECT flag when not every row shares the same
 *      imdbId — that's the signal of a likely false-merge (two different
 *      films share a sanitized form).
 *   3. Summary: total rows, total groups, groups with >1 row, max group
 *      size, count of suspect groups.
 *
 * Read with the merge-key rule we plan to ship in mind. If the suspect
 * groups all turn out to actually be the same film (TMDB just hadn't
 * resolved both rows yet), the rule is fine. If any suspect group contains
 * genuinely different films, the rule needs tightening before phase 2.
 *
 * Run: sbt "Test/runMain scripts.MergeKeyAudit"
 */
object MergeKeyAudit {
  def main(args: Array[String]): Unit = {
    val repo = new MovieRepo()
    if (!repo.enabled) {
      println("MONGODB_URI not set — nothing to audit.")
      sys.exit(1)
    }

    val rows = repo.findAll()
    println(s"@@ ${rows.size} enrichment rows read from Mongo")

    // Corpus = every stored cleanTitle. In production the corpus will be
    // (stored variants ∪ this-tick titles), but for an audit against the
    // current persisted state, the stored cleanTitles are the closest
    // analogue to "what the merge rule will see at write time."
    val keyFor = TitleNormalizer.mergeKeyLookup(rows.map(_._1))

    // Group by (merge-key, year). Year stays part of the key because today's
    // docId is `${normalize(title)}|${year}` — same-titled films from
    // different years are independent records.
    val groups: Map[(String, Option[Int]), Seq[(String, Option[Int], models.MovieRecord)]] =
      rows.groupBy { case (title, year, _) => (keyFor(title), year) }

    val multi      = groups.filter { case (_, rs) => rs.size > 1 }
    val sortedDesc = multi.toSeq.sortBy { case (_, rs) => -rs.size }

    println(s"@@ ${groups.size} distinct (mergeKey, year) groups")
    println(s"@@ ${multi.size} groups have >1 row (potential collapse)")
    println()

    var suspect = 0

    sortedDesc.foreach { case ((key, year), rs) =>
      val imdbIds = rs.flatMap(_._3.imdbId).toSet
      val isSuspect = imdbIds.size > 1 || (imdbIds.isEmpty && rs.size > 1)
      if (isSuspect) suspect += 1

      val flag = if (isSuspect) "SUSPECT" else "ok    "
      println(s"@@ [$flag] key='$key'  year=${year.getOrElse("?")}  rows=${rs.size}  imdbIds=${imdbIds.size}")
      rs.foreach { case (title, _, e) =>
        val imdb = e.imdbId.getOrElse("—")
        val tmdb = e.tmdbId.map(_.toString).getOrElse("—")
        val orig = e.originalTitle.getOrElse("—")
        println(s"@@     · '$title'  imdb=$imdb  tmdb=$tmdb  orig='$orig'")
      }
      println()
    }

    val maxGroupSize = if (multi.nonEmpty) multi.values.map(_.size).max else 1
    println(s"════ Summary ════")
    println(s"  total rows:                    ${rows.size}")
    println(s"  distinct groups:               ${groups.size}")
    println(s"  groups with >1 row:            ${multi.size}")
    println(s"  rows that would collapse:      ${multi.values.map(_.size).sum} → ${multi.size}" +
            s" (saves ${multi.values.map(_.size).sum - multi.size} rows)")
    println(s"  max group size:                $maxGroupSize")
    println(s"  SUSPECT groups (imdbId clash): $suspect")
    if (suspect > 0)
      println("  → review the SUSPECT groups above; each is either a real merge")
    else
      println("  → no imdbId clashes — every collapsing group is internally consistent.")

    repo.close()
  }
}

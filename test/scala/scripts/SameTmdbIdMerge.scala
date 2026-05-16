package scripts

import services.movies.{MongoMovieRepo, MovieRecordMerge, StoredMovieRecord}
import tools.Env

/**
 * One-shot backfill: collapse legacy same-tmdbId duplicates in the `movies`
 * collection — rows that were created before the `MovieCache.put` tmdbId gate
 * landed, and which the gate can't reach now because none of them is being
 * freshly re-resolved.
 *
 * For every group of rows sharing the same `tmdbId`:
 *   - **Canonical pick**: the row whose `year` matches the median scrape-
 *     reported year across all rows in the group (best guess for the
 *     cinema's "true" year). On ties, prefer the row whose `year` is
 *     non-None; among non-None years, the lowest. Otherwise first by `_id`.
 *   - **Merge**: every victim's `cinemaScrapes` + `cinemaShowings` are
 *     unioned onto the canonical via `MovieRecordMerge.union` — same merge
 *     primitive the runtime gate uses, so the offline cleanup and the
 *     online prevention agree on semantics.
 *   - **Persist**: the canonical row is re-upserted (with the unioned
 *     cinema-side data); each victim's `(title, year)` is deleted.
 *
 * Prints BEFORE → AFTER per group + a final throughput line. Run via:
 *
 *   sbt "Test/runMain scripts.SameTmdbIdMerge"
 */
object SameTmdbIdMerge {
  def main(args: Array[String]): Unit = {
    if (Env.get("MONGODB_URI").isEmpty) {
      println("MONGODB_URI not set."); sys.exit(1)
    }
    val repo  = new MongoMovieRepo()
    val start = System.currentTimeMillis()

    val all = repo.findAll()
    println(s"@@ ${all.size} doc(s) total")

    val groups = all
      .filter(_.record.tmdbId.isDefined)
      .groupBy(_.record.tmdbId.get)
      .filter { case (_, rows) => rows.size > 1 }

    println(s"@@ ${groups.size} tmdbId group(s) with duplicates")
    println()

    var merged  = 0
    var deleted = 0
    groups.toSeq.sortBy(_._1).foreach { case (tid, rows) =>
      val canonical = pickCanonical(rows)
      val victims   = rows.filterNot(r => sameKey(r, canonical))

      println(s"── tmdbId=$tid (${rows.size} rows)")
      println(s"   canonical: '${canonical.title}' (${canonical.year.getOrElse("—")})")

      val foldedRecord = victims.foldLeft(canonical.record) { (acc, v) =>
        println(s"     ← merging '${v.title}' (${v.year.getOrElse("—")})" +
                s"  cinemaScrapes=${v.record.cinemaScrapes.size}" +
                s" cinemaShowings=${v.record.cinemaShowings.size}")
        MovieRecordMerge.union(acc, v.record)
      }

      println(s"   BEFORE: canonical cinemaScrapes=${canonical.record.cinemaScrapes.size} " +
              s"cinemaShowings=${canonical.record.cinemaShowings.size}")
      println(s"   AFTER : canonical cinemaScrapes=${foldedRecord.cinemaScrapes.size} " +
              s"cinemaShowings=${foldedRecord.cinemaShowings.size}")

      repo.upsert(canonical.title, canonical.year, foldedRecord)
      victims.foreach { v =>
        repo.delete(v.title, v.year)
        deleted += 1
      }
      merged += 1
      println()
    }

    repo.close()
    val secs = (System.currentTimeMillis() - start) / 1000.0
    println(f"done in $secs%.1fs — $merged group(s) merged, $deleted victim row(s) deleted")
  }

  /** Two `StoredMovieRecord`s point to the same Mongo `_id` iff their
   *  (title, year) match exactly — the docId formula is deterministic on
   *  those two fields. */
  private def sameKey(a: StoredMovieRecord, b: StoredMovieRecord): Boolean =
    a.title == b.title && a.year == b.year

  /** Pick the row that best represents the canonical (title, year) for the
   *  group. Priority: non-None year > None year; among non-None, the lowest
   *  year wins (canonical release years tend to be older than re-release /
   *  parser-glitch years). Stable on ties via `_id`-style ordering. */
  private def pickCanonical(rows: Seq[StoredMovieRecord]): StoredMovieRecord = {
    rows.sortBy { r =>
      // (year-priority: 0 if Some, 1 if None; year value; title)
      val yearPriority = if (r.year.isDefined) 0 else 1
      val yearValue    = r.year.getOrElse(Int.MaxValue)
      (yearPriority, yearValue, r.title)
    }.head
  }
}

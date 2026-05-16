package scripts

import models.MovieRecord
import services.movies.{MongoMovieRepo, MovieService}

/**
 * Audit duplicate rows across alternative merge keys.
 *
 * The (title, year) docId we ship today produces duplicates whenever cinemas
 * disagree on year reporting — `("Top Gun: Maverick", Some(2022))` and
 * `("Top Gun: Maverick", None)` are separate rows because year is part of the
 * docId. Same film, two rows, two cards on /debug/enrichment.
 *
 * This script does NOT mutate Mongo. It reads every row and reports what
 * each merge strategy would collapse:
 *
 *   1. **By cleanTitle alone (drop year)** — most aggressive. Risks
 *      collapsing legitimate same-title-different-year films (Carrie 1976
 *      vs Carrie 2013) into one row.
 *
 *   2. **By (cleanTitle, director)** — uses cinema-reported director as the
 *      tie-breaker for sequels / remakes that share a title. Director comes
 *      from `e.director` (the merged cinema-side value).
 *
 *   3. **By imdbId** — the gold standard. Two rows that TMDB resolved to
 *      the same IMDb id are definitionally the same film. Reported for
 *      comparison; folds Polish/Cyrillic cross-script translations too.
 *
 * For each strategy, prints:
 *   - groups with >1 row
 *   - how many would collapse cleanly (consistent imdbId across the group)
 *   - how many groups have conflicting imdbId (the "SUSPECT" case — same
 *     title key, different films — strategy is unsafe)
 *   - leftover rows that no strategy could collapse
 *
 * Run: sbt "Test/runMain scripts.DuplicateAudit"
 */
object DuplicateAudit {
  def main(args: Array[String]): Unit = {
    val repo = new MongoMovieRepo()
    if (!repo.enabled) { println("MONGODB_URI not set."); sys.exit(1) }

    val rows: Seq[(String, Option[Int], MovieRecord)] = repo.findAll()
    println(s"@@ ${rows.size} rows read")
    println()

    // Helpers — the sanitized title is the same form the production docId
    // already uses, just without the |year suffix.
    def titleKey(t: String): String = MovieService.normalize(t)
    def directorKey(d: Option[String]): String =
      d.map(MovieService.normalize).getOrElse("")

    auditStrategy("Strategy 1: by sanitized title alone (drop year)", rows) { case (t, _, _) =>
      titleKey(t)
    }
    auditStrategy("Strategy 2: by (sanitized title, normalized director)", rows) { case (t, _, e) =>
      s"${titleKey(t)}|${directorKey(e.director)}"
    }
    auditStrategy("Strategy 3: by imdbId (only rows with imdbId)",
                  rows.filter { case (_, _, e) => e.imdbId.isDefined }) { case (_, _, e) =>
      e.imdbId.get
    }

    // Cross-strategy: how many rows would title-only merge collapse if we
    // ALSO required imdbId consistency (the safe subset of strategy 1)?
    println()
    println(s"════ Safe title-only merge ════")
    val safeMerges = rows
      .groupBy { case (t, _, _) => titleKey(t) }
      .filter { case (_, group) =>
        val imdbIds = group.flatMap(_._3.imdbId).toSet
        group.size > 1 && imdbIds.size <= 1   // either all-same-imdbId or all-None
      }
    val safeRows = safeMerges.values.map(_.size).sum
    println(s"  ${safeMerges.size} title groups would safely collapse")
    println(s"    → $safeRows rows → ${safeMerges.size} rows (saves ${safeRows - safeMerges.size})")

    // Title groups where merging is UNSAFE (different imdbIds → different films
    // sharing a title key).
    val unsafe = rows
      .groupBy { case (t, _, _) => titleKey(t) }
      .filter { case (_, group) =>
        val imdbIds = group.flatMap(_._3.imdbId).toSet
        group.size > 1 && imdbIds.size > 1
      }
    if (unsafe.nonEmpty) {
      println()
      println(s"════ Unsafe title-only collapses (different imdbIds — would corrupt) ════")
      unsafe.toSeq.sortBy(-_._2.size).foreach { case (key, group) =>
        println(s"  key='$key'  rows=${group.size}  imdbIds=${group.flatMap(_._3.imdbId).toSet.mkString(", ")}")
        group.foreach { case (t, y, e) =>
          println(s"    · '$t' (${y.getOrElse("?")})  imdb=${e.imdbId.getOrElse("—")}  director=${e.director.getOrElse("—")}")
        }
      }
    }

    repo.close()
  }

  private def auditStrategy(
    label: String,
    rows:  Seq[(String, Option[Int], MovieRecord)]
  )(key: ((String, Option[Int], MovieRecord)) => String): Unit = {
    println(s"════ $label ════")
    val groups = rows.groupBy(key)
    val multi  = groups.filter(_._2.size > 1)
    val total  = multi.values.map(_.size).sum
    val rowsAfterCollapse = (rows.size - multi.values.map(_.size).sum) + multi.size

    var consistent  = 0
    var inconsistent = 0
    multi.foreach { case (_, group) =>
      val imdbIds = group.flatMap(_._3.imdbId).toSet
      if (imdbIds.size <= 1) consistent += 1 else inconsistent += 1
    }

    println(s"  total rows:                 ${rows.size}")
    println(s"  groups with >1 row:         ${multi.size}")
    println(s"  rows that would collapse:   $total → ${multi.size} (saves ${total - multi.size})")
    println(s"  rows after collapse total:  $rowsAfterCollapse")
    println(s"  groups consistent imdbId:   $consistent  (safe to merge)")
    println(s"  groups inconsistent imdbId: $inconsistent (different films — unsafe)")
    println()
  }
}

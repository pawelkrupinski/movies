package scripts

import services.movies.{MongoMovieRepo, MovieService, StoredMovieRecord}

/**
 * One-shot cleanup for duplicate rows produced by the asyncHydrate vs.
 * first-scrape race (the runtime fix in commit 91e7ad7 stops new
 * duplicates; this clears the residue already in Mongo).
 *
 * Detection: a row is treated as a "buggy twin" when it shares a
 * normalised cleanTitle with another row that carries TMDB enrichment
 * (tmdbId set), but the twin itself has NO enrichment (no tmdbId, no
 * imdbId). The twin is necessarily a scraper-only row that the race
 * created at a stale (title, year) key — either year=None, or a
 * cinema-reported wrong year that disagrees with the canonical.
 *
 * Action per group:
 *   - canonical = the row with tmdbId (the one that's been TMDB-resolved)
 *   - merge every twin's cinema slots into the canonical (canonical wins
 *     on per-cinema collisions because its data is fresher in practice —
 *     the periodic refresh keeps re-writing it)
 *   - upsert the merged canonical
 *   - delete every twin
 *
 * Run: sbt "Test/runMain scripts.DedupeYearlessRace"          # dry-run
 *      sbt "Test/runMain scripts.DedupeYearlessRace --apply"  # mutate
 */
object DedupeYearlessRace {
  def main(args: Array[String]): Unit = {
    val apply = args.contains("--apply")
    val repo  = new MongoMovieRepo()
    if (!repo.enabled) { println("MONGODB_URI not set."); sys.exit(1) }

    val rows = repo.findAll()
    println(s"@@ ${rows.size} rows read; mode = ${if (apply) "APPLY" else "DRY RUN"}")
    println()

    val byNorm = rows.groupBy(r => MovieService.normalize(r.title))

    var mergedGroups = 0
    var twinsDropped = 0
    var skippedGroups = 0

    println("=" * 100)
    println(f"${"Action"}%-7s ${"Title"}%-40s ${"Year"}%-6s ${"tmdbId"}%-10s ${"imdbId"}%-12s Cinemas")
    println("=" * 100)

    byNorm.toSeq.sortBy(_._1).foreach { case (normTitle, group) =>
      if (group.size > 1) {
        val canonical = group.find(_.record.tmdbId.isDefined)
        canonical match {
          case Some(c) =>
            val twins = group.filterNot(_ == c).filter(r =>
              r.record.tmdbId.isEmpty && r.record.imdbId.isEmpty
            )
            val nonMergeable = group.filterNot(_ == c).filterNot(twins.contains)
            if (twins.nonEmpty) {
              println(f"${"KEEP"}%-7s ${c.title}%-40s ${c.year.getOrElse("—").toString}%-6s ${c.record.tmdbId.getOrElse("—").toString}%-10s ${c.record.imdbId.getOrElse("—")}%-12s ${slotSummary(c)}")
              twins.foreach { r =>
                println(f"${"DROP"}%-7s ${r.title}%-40s ${r.year.getOrElse("—").toString}%-6s ${r.record.tmdbId.getOrElse("—").toString}%-10s ${r.record.imdbId.getOrElse("—")}%-12s ${slotSummary(r)}")
              }
              nonMergeable.foreach { r =>
                println(f"${"SKIP"}%-7s ${r.title}%-40s ${r.year.getOrElse("—").toString}%-6s ${r.record.tmdbId.getOrElse("—").toString}%-10s ${r.record.imdbId.getOrElse("—")}%-12s ${slotSummary(r)} (has enrichment)")
              }
              println("-" * 100)
              mergedGroups += 1
              twinsDropped += twins.size
              if (nonMergeable.nonEmpty) skippedGroups += 1
            } else {
              // Has duplicates but none match the twin pattern.
              println(f"${"NOOP"}%-7s ${normTitle}%-40s     -- no twins without enrichment to merge --")
              group.foreach { r =>
                println(f"  ${r.title}%-46s ${r.year.getOrElse("—").toString}%-6s ${r.record.tmdbId.getOrElse("—").toString}%-10s ${r.record.imdbId.getOrElse("—")}%-12s ${slotSummary(r)}")
              }
              println("-" * 100)
              skippedGroups += 1
            }
          case None =>
            // No canonical with tmdbId — can't tell which is "real". Leave alone.
            println(f"${"NOOP"}%-7s ${normTitle}%-40s     -- no row with tmdbId, can't pick canonical --")
            group.foreach { r =>
              println(f"  ${r.title}%-46s ${r.year.getOrElse("—").toString}%-6s ${r.record.tmdbId.getOrElse("—").toString}%-10s ${r.record.imdbId.getOrElse("—")}%-12s ${slotSummary(r)}")
            }
            println("-" * 100)
            skippedGroups += 1
        }
      }
    }

    println()
    println(s"@@ Summary:")
    println(s"   ${mergedGroups} group(s) ${if (apply) "merged" else "would merge"}")
    println(s"   ${twinsDropped} twin row(s) ${if (apply) "deleted" else "would be deleted"}")
    println(s"   ${skippedGroups} group(s) skipped (no canonical, or duplicates carry own enrichment)")

    if (apply) {
      byNorm.values.foreach { group =>
        if (group.size > 1) {
          group.find(_.record.tmdbId.isDefined).foreach { c =>
            val twins = group.filterNot(_ == c).filter(r =>
              r.record.tmdbId.isEmpty && r.record.imdbId.isEmpty
            )
            if (twins.nonEmpty) {
              val mergedData = twins.foldLeft(c.record.data) {
                case (acc, r) => r.record.data ++ acc
              }
              repo.upsert(c.title, c.year, c.record.copy(data = mergedData))
              twins.foreach(r => repo.delete(r.title, r.year))
            }
          }
        }
      }
      println()
      println("APPLIED.")
    } else {
      println()
      println("Re-run with --apply to actually mutate Mongo.")
    }

    repo.close()
  }

  private def slotSummary(r: StoredMovieRecord): String = {
    val names = r.record.data.keys.map(_.displayName).toSeq.sorted
    s"[${names.mkString(", ")}]"
  }
}

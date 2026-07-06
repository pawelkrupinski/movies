package scripts

import clients.TmdbClient
import services.events.InProcessEventBus
import services.movies.{CaffeineMovieCache, EmbeddedYear, MongoMovieRepository, MovieService, StoredMovieRecord}
import tools.RealHttpFetch

/**
 * One-shot: bring EXISTING yearless rows up to what the scrape-boundary
 * `EmbeddedYear` persist now produces. For every row stored with no year whose
 * cinema-slot titles carry an unambiguous delimited year ("Konwicki: Lawa (1989)",
 * "Following (1998)", "Pan Tadeusz - 1999"), re-key it onto that year, stamp the
 * year onto its yearless slots (via the vetted `MovieCache.backfillEmbeddedYears`,
 * which merges into any existing yeared sister row), then re-enrich under the new
 * key so the now-yeared row resolves against TMDB.
 *
 * Reads MONGODB_URI / TMDB_API_KEY from `.env.local` via tools.Env.
 *
 * Dry-run by default (prints the plan, mutates nothing). Pass `--apply` to write.
 *
 * Run with: sbt "worker/Test/runMain scripts.EmbeddedYearBackfill [--apply]"
 */
object EmbeddedYearBackfill {
  def main(args: Array[String]): Unit = {
    val apply      = args.contains("--apply")
    val repository = new MongoMovieRepository()
    if (!repository.enabled) {
      println("MONGODB_URI not set — nothing to backfill.")
      sys.exit(1)
    }

    // Candidate plan is a pure function of the stored rows — compute it directly
    // from the repository so the dry-run needs no cache/TMDB wiring.
    val candidates: Seq[(StoredMovieRecord, Int)] = repository.findAll().flatMap {
      case row if row.year.isEmpty =>
        EmbeddedYear.ofAll(row.record.data.values.flatMap(sd => sd.rawTitle ++ sd.title)).map(row -> _)
      case _ => None
    }.sortBy { case (row, _) => row.title.toLowerCase }

    println(s"${candidates.size} yearless row(s) carry an unambiguous delimited year:\n")
    candidates.foreach { case (row, year) => println(f"  → $year%4d   ${row.title}") }
    if (candidates.isEmpty) { println("\nNothing to do."); return }

    if (!apply) {
      println("\nDry run — pass --apply to re-key + re-enrich these rows.")
      return
    }

    val startedAtMs = System.currentTimeMillis()
    val cache       = new CaffeineMovieCache(repository)
    val tmdb        = new TmdbClient(new RealHttpFetch)
    val service     = new MovieService(cache, new InProcessEventBus(), tmdb)

    val moved = cache.backfillEmbeddedYears()
    println(s"\nRe-keyed $moved row(s). Re-enriching under the new year…")

    // Re-enrich each moved row under its NEW (title, year) so the yeared row
    // resolves against TMDB (the id/ratings the year unblocks).
    candidates.foreach { case (row, year) =>
      service.reEnrichSync(row.title, Some(year))
      val after = service.get(row.title, Some(year))
      println(f"  ${row.title} ($year): tmdbId=${after.flatMap(_.tmdbId).getOrElse("—")} imdbId=${after.flatMap(_.imdbId).getOrElse("—")}")
    }

    val secs = (System.currentTimeMillis() - startedAtMs) / 1000.0
    println(f"\nDone in $secs%.1fs · $moved row(s) re-keyed.")
  }
}

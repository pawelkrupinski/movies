package scripts

import models.{KinoMuza, Source}
import services.movies.MongoMovieRepo

/**
 * One-shot: clear `synopsis` (and `trailerUrl` + `posterUrl`) on every
 * Kino Muza slot in Mongo so the `KinoMuzaSynopsisRefresher` walks the
 * full corpus again and pulls fresh data from each film's detail page.
 *
 * Why all three: `synopsis.isEmpty` is the refresher's candidate-filter
 * trigger, but the same per-row tick now also writes trailer + poster
 * from the same detail-page fetch. Clearing all three together means
 * the refresh writes fresh values for everything in one pass — most
 * notably the new portrait poster (`KinoMuzaClient.parsePoster`) that
 * the listing-scrape never sees.
 *
 * Output: per-row "title  [what was set] → cleared" plus a total count
 * and a copy-pasteable curl to kick the in-memory cache.
 *
 * After running, the cache on prod still has the pre-clear synopses; a
 * `POST /debug/rehydrate` reloads the cache from Mongo so the refresher
 * sees the cleared rows as candidates.
 *
 * Run: sbt "Test/runMain scripts.MuzaSynopsisClear"
 *
 * Delete after one successful prod run.
 */
object MuzaSynopsisClear {
  def main(args: Array[String]): Unit = {
    val repo = new MongoMovieRepo()
    if (!repo.enabled) {
      println("MONGODB_URI not set — nothing to clear.")
      sys.exit(1)
    }

    val all     = repo.findAll().sortBy(r => (r.title.toLowerCase, r.year))
    val touched = all.filter { r =>
      r.record.data.get(KinoMuza).exists(s =>
        s.synopsis.isDefined || s.trailerUrl.isDefined || s.posterUrl.isDefined
      )
    }

    println(s"${all.size} rows total in Mongo · ${touched.size} have a Muza slot with synopsis/trailer/poster set.\n")

    touched.foreach { r =>
      val s = r.record.data(KinoMuza)
      val had = Seq(
        s.synopsis.map(_ => "synopsis"),
        s.trailerUrl.map(_ => "trailer"),
        s.posterUrl.map(_ => "poster")
      ).flatten.mkString("+")
      val updated = r.record.copy(
        data = r.record.data + ((KinoMuza: Source) -> s.copy(
          synopsis   = None,
          trailerUrl = None,
          posterUrl  = None
        ))
      )
      repo.upsert(r.title, r.year, updated)
      println(f"  ${r.title}%-60s [$had%s → cleared]")
    }

    println(s"\nDone. ${touched.size} Muza slot(s) cleared.")
    println("Trigger the in-memory cache reload so the refresher sees the cleared rows:")
    println("  curl -X POST https://kinowo.fly.dev/debug/rehydrate")
    println("\nRefresher then processes ~1 row per minute via KinoMuzaSynopsisRefresher.")

    repo.close()
  }
}

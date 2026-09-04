package scripts

import org.mongodb.scala.MongoClient
import services.movies.{MongoMovieRepository, MongoScreeningsRepository, MongoSlotsRepository, SingleCountryNormalizer}
import tools.Env

/**
 * One-off: re-file the venues of a KNOWN set of rows split by the Kinoteka bad-year-hint
 * bug (`KinotekaClient.fetchFilmDetail`, fixed in 39f9014d6 / 832133b6f) onto the sibling
 * row that is actually the same film, and drop the now-empty split row.
 *
 * That fix stops NEW rows from being poisoned, but a detail refresh merges into the
 * row's EXISTING key (`EnrichDetailsHandler.handle`, `rowKey = cache.keyOf(title, year)`)
 * — nothing re-derives an already-wrong key from corrected data, and `SettleReaper`'s
 * `canonicalizeBySanitize` only clusters rows by their EXISTING key year. So the rows
 * below are stuck at their poisoned key forever without an explicit move. This script IS
 * that move, once, for the specific pairs confirmed by hand on 2026-08-28 (prod PL):
 *
 *   - `harrypotterikamienfilozoficzny|2026` (35 Multikino venues, tmdbNoMatch, year
 *     poisoned by Kinoteka's "(dubbing PL)" listing reporting a 2026 "premiere") →
 *     `harrypotterikamienfilozoficzny25rocznicapremiery|2001` (tmdbId 671, resolved).
 *     Both search-titles now match after the 39f9014d6 "rocznica premiery" strip; the
 *     poisoned year (25 years outside clusterByFilm's ±2 window) is the only reason the
 *     periodic settle never folded them.
 *   - `kupilemmotocyklktoryjestwampiremnajlepszeznajgorszych|2026` → the resolved
 *     `kupilemmotocyklktoryjestwampirem|1990` (tmdbId 41153) — same shape, closed by the
 *     832133b6f "Najlepsze z Najgorszych [seans bez reklam]" strip.
 *   - `harrypotteriinsygniasmierciczesc2|2026` → the resolved
 *     `harrypotteriinsygniasmiercicz2|2011` (tmdbId 12445, Harry Potter and the Deathly
 *     Hallows: Part 2) — its own cinemaTitles already carry "Część II" as an accepted
 *     spelling, so "Część 2 (dubbing PL)" is the same film under a numeral variant, not a
 *     title-rule gap; a fresh yearless TMDB search will confirm it going forward.
 *   - `harrypotteriinsygniasmierciczesc1|2026` → the (also still-unresolved) yearless
 *     `harrypotteriinsygniasmiercicz1|` — no resolved anchor exists yet for Part 1, so this
 *     only collapses two unresolved cards into one; TMDB resolution is unaffected either
 *     way and can still happen on either row after the merge.
 *
 * SAFETY. Uses ONLY the existing, tested move primitive (`MovieRepository.moveFilm` →
 * `SideCollectionMove.move`): it reads both the source and destination side-collection
 * rows, writes their UNION to the destination, verifies both the moved AND the
 * destination's own rows actually landed, and returns false (refusing to proceed) on any
 * read/write uncertainty — never overwrites, never deletes on a partial write. This script
 * only ever calls `deleteById` on the source `movies` document, and only after `moveFilm`
 * reports true (the side-collection rows are confirmed safely at the destination).
 *
 * Dry run by DEFAULT — pass `--apply` to write.
 *
 *   . scripts/local-mirror/prod-tunnel.sh && ensure_prod_tunnel   # ssh forward to mongo-1
 *   MONGODB_DB=kinowo sbt "worker/Test/runMain scripts.MergeKinotekaPoisonedRows"
 *   MONGODB_DB=kinowo sbt "worker/Test/runMain scripts.MergeKinotekaPoisonedRows --apply"
 */
object MergeKinotekaPoisonedRows {

  // (old id — the poisoned/split row, new id — the row it should merge onto)
  val pairs: Seq[(String, String)] = Seq(
    "harrypotterikamienfilozoficzny|2026"                          -> "harrypotterikamienfilozoficzny25rocznicapremiery|2001",
    "kupilemmotocyklktoryjestwampiremnajlepszeznajgorszych|2026"   -> "kupilemmotocyklktoryjestwampirem|1990",
    "harrypotteriinsygniasmierciczesc2|2026"                       -> "harrypotteriinsygniasmiercicz2|2011",
    "harrypotteriinsygniasmierciczesc1|2026"                       -> "harrypotteriinsygniasmiercicz1|"
  )

  def main(args: Array[String]): Unit = {
    val apply      = args.contains("--apply")
    val normalizer = SingleCountryNormalizer.titleNormalizer
    val uri        = Env.get("MONGODB_URI").getOrElse { println("MONGODB_URI not set."); sys.exit(1) }
    val dbName     = Env.get("MONGODB_DB").getOrElse("kinowo")
    val client     = MongoClient(uri)
    val db         = client.getDatabase(dbName)
    val repository = new MongoMovieRepository(Some(db), fallbackToOwnInit = false,
      screenings = Some(new MongoScreeningsRepository(Some(db))),
      slots      = Some(new MongoSlotsRepository(Some(db))),
      normalizer = normalizer)
    if (!repository.enabled) { println(s"movies repository not enabled for $dbName."); sys.exit(1) }
    println(s"MergeKinotekaPoisonedRows — ${if (apply) "APPLY" else "DRY RUN"} (db=$dbName)")

    pairs.foreach { case (oldId, newId) =>
      println()
      println(s"$oldId  ->  $newId")
      if (!apply) {
        println("  (dry run — re-run with --apply to move screenings/slots and delete the source row)")
      } else if (repository.moveFilm(oldId, newId)) {
        repository.deleteById(oldId)
        println(s"  merged and deleted $oldId")
      } else {
        println(s"  DEFERRED — moveFilm refused (a read/write did not verify); left both rows untouched")
      }
    }

    println()
    println(if (apply) "done." else "dry run — nothing written. Re-run with --apply.")
    repository.close()
    client.close()
  }
}

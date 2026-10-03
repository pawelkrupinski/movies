package scripts

import models.Country
import services.movies.{MongoMovieRepository, MongoScreeningsRepository, MongoSlotsRepository, MovieRepository, ScreeningsRepository, SlotsRepository, TitleNormalizer}

/**
 * One-shot repair: give a film the `movie_slots` rows its showtimes need, from the slots still
 * EMBEDDED in its `movies` document.
 *
 * A film whose slots never reached `movie_slots` (written before the split, or whose slot write
 * failed) and that has since been touched only by `updateIfPresent` — which writes showtime rows but
 * never slot rows — has `screenings` rows with no slot row beside them. The venue-scoped read
 * (`MovieRepository.readVenues`) declines such a film, and the change stream falls back to a whole-film
 * re-read every time one of its venues changes: `kinowo_worker_change_apply_total{reason=
 * "venue_read_failed"}`, ~133 a day across UK/PL/DE (2026-10-02). The whole-film read already serves
 * the embedded slot for those keys (`SlotsRepository.merge`), so writing it as a row changes what is
 * served nowhere — it only lets the venue read see it.
 *
 * Writes ONLY a missing row, and only for a key that HAS showtimes: never replaces a row (the embedded
 * copy is stale wherever a row exists), never resurrects a slot whose venue no longer lists the film.
 * Idempotent; safe beside a live worker. Reads through a movie repository with no slots wired, so the
 * record's `data` is the embedded map, not a stitched one.
 *
 * Dry run by default; pass --apply to write. Country codes narrow it (default: every country).
 *   . scripts/local-mirror/prod-tunnel.sh && ensure_prod_tunnel
 *   sbt "worker/Test/runMain scripts.AddMissingMovieSlots uk pl de"            # dry run
 *   sbt "worker/Test/runMain scripts.AddMissingMovieSlots --apply uk pl de"
 */
object AddMissingMovieSlots {

  final case class Counts(films: Int = 0, repaired: Int = 0, rows: Int = 0, unread: Int = 0) {
    def +(other: Counts): Counts = Counts(films + other.films, repaired + other.repaired, rows + other.rows, unread + other.unread)
    def describe: String = s"$films film(s) scanned, $repaired with missing slot rows, $rows row(s), $unread unreadable"
  }

  /** Pure over the traits. `complete` is false when the corpus scan aborted mid-way. */
  def run(movies: MovieRepository, slots: SlotsRepository, screenings: ScreeningsRepository, apply: Boolean): (Counts, Boolean) = {
    var counts = Counts()
    val complete = movies.foreachRecord { row =>
      counts = counts.copy(films = counts.films + 1)
      val embedded = SlotsRepository.slotsOf(row.record.data)
      if (embedded.nonEmpty) {
        val id                  = row.id.value
        slots.findForFilmChecked(id).flatMap(stored => screenings.findForFilmChecked(id).map(stored -> _)).answered match {
          case None => counts = counts.copy(unread = counts.unread + 1)
          case Some((stored, shown)) =>
            val missing = embedded.filter { case (key, _) => shown.contains(key) && !stored.contains(key) }
            if (missing.nonEmpty) {
              if (apply) missing.foreach { case (key, slot) => slots.upsertSlot(id, key, slot) }
              counts = counts.copy(repaired = counts.repaired + 1, rows = counts.rows + missing.size)
            }
        }
      }
    }
    (counts, complete.isComplete)
  }

  def main(args: Array[String]): Unit = {
    val apply     = args.contains("--apply")
    val requested = args.filterNot(_.startsWith("--")).toSeq
    val countries =
      if (requested.isEmpty) Country.all
      else requested.flatMap(code => Country.byCode(code).orElse { println(s"Unknown country code '$code'."); sys.exit(1) })
    println(if (apply) "APPLY — missing slot rows will be WRITTEN." else "DRY RUN — nothing is written. Pass --apply to write.")
    var incomplete = false
    countries.foreach { country =>
      val (connection, database) = CountryDatabase.open(country)
      val screenings = new MongoScreeningsRepository(Some(database))
      // Slots deliberately NOT wired: the record's `data` must be the embedded map.
      val movies     = new MongoMovieRepository(Some(database), screenings = Some(screenings), normalizer = TitleNormalizer.forCountry(country))
      val (counts, complete) = run(movies, new MongoSlotsRepository(Some(database)), screenings, apply)
      println(s"${country.displayName} (${country.mongoDb}): ${counts.describe}${if (complete) "" else " — SCAN INCOMPLETE, re-run"}")
      incomplete ||= !complete
      connection.close()
    }
    sys.exit(if (incomplete) 2 else 0)
  }
}

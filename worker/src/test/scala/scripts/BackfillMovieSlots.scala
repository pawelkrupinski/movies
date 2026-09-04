package scripts

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{Source, SourceData}
import org.mongodb.scala.MongoClient
import services.movies.{MongoMovieRepository, MongoScreeningsRepository, MongoSlotsRepository, MovieRepository, SlotsRepository, StoredMovieRecord}
import tools.Env

/**
 * One-shot backfill: copy every film's per-cinema `SourceData` out of the embedded
 * `movies.sourceData` map into its own `movie_slots` rows.
 *
 * The split is LAZY — a film only gains rows when it is next written — so without
 * this a film nobody re-scrapes keeps reading from the embedded map indefinitely,
 * and the embedded copy can never be retired. This closes that tail in one pass.
 *
 * Idempotent, and safe to run against a live worker: it writes exactly what the
 * worker's own dual write would (`replaceFilm` per film, same `_id` formula), so the
 * two agree wherever they overlap and a re-run converges to the same state. It only
 * ADDS to `movie_slots`; nothing in `movies` is modified or deleted.
 *
 * Reads through a repository with NO slots wired, deliberately: this must copy what
 * the EMBEDDED map holds, not what a slots-aware read would stitch back (which for an
 * already-migrated film is the rows we are trying to write, making the pass a no-op
 * that silently skips the films it exists to fix).
 *
 * Run against prod via the ssh tunnel (see reference_prod_mongo_access), once per
 * country database:
 *   . scripts/local-mirror/prod-tunnel.sh && ensure_prod_tunnel   # ssh forward to mongo-1
 *   MONGODB_DB=kinowo    sbt "worker/Test/runMain scripts.BackfillMovieSlots"
 *   MONGODB_DB=kinowo_uk sbt "worker/Test/runMain scripts.BackfillMovieSlots"
 *   MONGODB_DB=kinowo_de sbt "worker/Test/runMain scripts.BackfillMovieSlots"
 * Add `--dry-run` to report what it would write without writing.
 */
object BackfillMovieSlots {

  /** Copy each film's embedded slots into `slots`. Streams the corpus row by row
   *  (`foreachRecord`) rather than materialising it — the corpus is ~13 MB and this
   *  runs on the worker box. Returns (filmsScanned, filmsWritten, slotsWritten);
   *  `complete` is false when the scan aborted mid-way, in which case the caller must
   *  NOT treat the result as full coverage. Pure over the traits, so the spec drives
   *  it with in-memory repositories. */
  def run(movieRepository: MovieRepository, slots: SlotsRepository, dryRun: Boolean): (Int, Int, Int, Boolean) = {
    var scanned = 0
    var written = 0
    var slotRows = 0
    val complete = movieRepository.foreachRecord { row =>
      scanned += 1
      val embedded: Map[Source, SourceData] = row.record.data
      if (embedded.nonEmpty) {
        val id      = StoredMovieRecord.idOf(row, titleNormalizer)
        val payload = SlotsRepository.slotsOf(embedded)
        if (!dryRun) slots.replaceFilm(id, payload)
        written  += 1
        slotRows += payload.size
      }
    }
    (scanned, written, slotRows, complete)
  }

  def main(args: Array[String]): Unit = {
    val dryRun = args.contains("--dry-run")
    val uri = Env.get("MONGODB_URI").getOrElse { println("MONGODB_URI not set."); sys.exit(1) }
    val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")
    val client = MongoClient(uri)
    try {
      val db = client.getDatabase(dbName)
      // Screenings wired so each row reads complete; slots deliberately NOT (see above).
      val screenings      = new MongoScreeningsRepository(Some(db))
      val movieRepository = new MongoMovieRepository(Some(db), fallbackToOwnInit = false, screenings = Some(screenings), normalizer = titleNormalizer)
      val slots           = new MongoSlotsRepository(Some(db))
      require(movieRepository.enabled, s"movies repository not enabled for $dbName")

      println(s"BackfillMovieSlots: $dbName${if (dryRun) " (dry run)" else ""}")
      val (scanned, written, slotRows, complete) = run(movieRepository, slots, dryRun)
      println(s"  scanned $scanned film(s), ${if (dryRun) "would write" else "wrote"} $written film(s) / $slotRows slot row(s)")
      if (!complete) {
        println("  SCAN INCOMPLETE — a batch failed after retries. This is NOT full coverage; re-run before")
        println("  treating the corpus as migrated (the embedded map must not be retired on a partial pass).")
        sys.exit(2)
      }
    } finally client.close()
  }
}

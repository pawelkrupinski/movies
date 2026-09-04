package scripts

import services.movies.SingleCountryNormalizer.titleNormalizer

import org.mongodb.scala.MongoClient
import services.movies.{MongoMovieRepository, MongoScreeningsRepository, MongoSlotsRepository, MovieRepository, SlotsRepository, StoredMovieRecord}
import tools.Env

/**
 * Post-deployment check for the `movies` → `movie_slots` split.
 *
 * This exists because the two worst bugs in that migration were invisible to the test
 * suite. Every spec passed while a metadata change silently stopped re-projecting, and
 * while `movies` documents were never actually shrinking — the paths were individually
 * correct and cancelled each other out. Only looking at deployed data showed it. So the
 * looking is automated rather than remembered.
 *
 * Checks, per country database:
 *  - FATAL: a film with slots in NEITHER `movie_slots` nor the embedded map. That film
 *    has no cinemas anywhere; it is the one unrecoverable state this migration can reach.
 *  - Coverage: how many films have slot rows, and how many still carry an embedded copy.
 *  - Progress: whether documents are actually shrinking. A corpus fully covered by
 *    `movie_slots` yet with nothing stripped means the write path is putting the embedded
 *    map straight back — the regression that made the whole split a no-op.
 *
 * Read-only. Exits non-zero on a FATAL finding so it can gate a deploy or run from cron.
 *
 *   . scripts/local-mirror/prod-tunnel.sh && ensure_prod_tunnel   # ssh forward to mongo-1
 *   MONGODB_DB=kinowo_uk sbt "worker/Test/runMain scripts.VerifyMovieSlots"
 */
object VerifyMovieSlots {

  case class Report(films: Int, withSlotRows: Int, withEmbedded: Int, withNeither: Seq[String], complete: Boolean) {
    def stripped: Int = films - withEmbedded
    def healthy:  Boolean = withNeither.isEmpty
    /** Covered but nothing stripped ⇒ the write path is re-adding the embedded map. */
    def stalled:  Boolean = films > 0 && withSlotRows > 0 && stripped == 0
  }

  /** Pure over the traits so the spec drives it with in-memory repositories. Reads the
   *  corpus through a repository with NO slots wired, so `record.data` is what `movies`
   *  actually stores rather than what a stitch would put back.
   *
   *  `complete` is false when EITHER read fell short, and every other number is then
   *  meaningless. Both directions matter and both used to be silently wrong: a failed
   *  `movie_slots` scan reads back as an empty map, which made every already-stripped
   *  film look like it had slots in NEITHER place — a corpus-wide FATAL, from a blip.
   *  A failed corpus scan delivered no rows at all, so `films=0`, nothing landed in
   *  `withNeither`, and the check printed OK. A verifier that cries outage on a blip and
   *  passes on an empty read is worse than none — this one refuses to render a verdict
   *  it cannot stand behind. */
  def run(embeddedView: MovieRepository, slots: SlotsRepository): Report = {
    val (stored, slotsComplete) = slots.findAllChecked()
    var films = 0
    var withSlotRows = 0
    var withEmbedded = 0
    val neither = Seq.newBuilder[String]
    val scanComplete = embeddedView.foreachRecord { row =>
      films += 1
      val id       = StoredMovieRecord.idOf(row, titleNormalizer)
      val hasRows  = stored.get(id).exists(_.nonEmpty)
      val hasInDoc = row.record.data.nonEmpty
      if (hasRows)  withSlotRows += 1
      if (hasInDoc) withEmbedded += 1
      if (!hasRows && !hasInDoc) neither += id
    }
    Report(films, withSlotRows, withEmbedded, neither.result(), slotsComplete && scanComplete)
  }

  def main(args: Array[String]): Unit = {
    val uri    = Env.get("MONGODB_URI").getOrElse { println("MONGODB_URI not set."); sys.exit(1) }
    val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")
    val client = MongoClient(uri)
    try {
      val db     = client.getDatabase(dbName)
      val scr    = new MongoScreeningsRepository(Some(db))
      val embedded = new MongoMovieRepository(Some(db), fallbackToOwnInit = false, screenings = Some(scr), normalizer = titleNormalizer)
      val slots  = new MongoSlotsRepository(Some(db))
      require(embedded.enabled, s"movies repository not enabled for $dbName")

      val r = run(embedded, slots)
      println(s"VerifyMovieSlots: $dbName")
      println(s"  films=${r.films}  with-slot-rows=${r.withSlotRows}  still-embedded=${r.withEmbedded}  stripped=${r.stripped}")
      if (!r.complete) {
        println("  INCONCLUSIVE: a corpus or movie_slots read fell short of the whole collection, so these")
        println("  counts are a partial view. Re-run — do NOT read a FATAL or an OK out of a partial scan.")
        sys.exit(3)
      }
      if (r.stalled)
        println("  WARNING: the corpus has slot rows but not one document has been stripped — " +
          "the write path is likely putting the embedded map back, which makes the split a no-op.")
      if (!r.healthy) {
        println(s"  FATAL: ${r.withNeither.size} film(s) have slots in NEITHER place — they have no cinemas at all:")
        r.withNeither.take(10).foreach(id => println(s"    $id"))
        sys.exit(2)
      }
      println("  OK: every film's slots are readable from one place or the other.")
    } finally client.close()
  }
}

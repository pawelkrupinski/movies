package scripts

import services.movies.SingleCountryNormalizer.titleNormalizer

import org.mongodb.scala.MongoClient
import services.movies.{MongoMovieRepository, MongoScreeningsRepository, MongoSlotsRepository, MovieRepository}
import services.readmodel.{MongoReadModelRepository, ReadModelProjection, ReadModelReader, ReadModelWriter}
import tools.Env

/**
 * One-shot backfill: populate the denormalised read-model collections
 * (`web_movies`, `web_screenings`) from the existing `movies` collection, so the
 * serving app can read from them the moment it deploys — no empty-repertoire
 * window before the worker's first projection lands.
 *
 * A FULL idempotent reconcile: every `movies` row is projected and upserted,
 * then any derived document not in the freshly-projected set is deleted (a film that
 * left the corpus, or a cinema that stopped screening one). Re-running converges
 * to the same state, and it's safe to run while the worker is live — the
 * projector's incremental writes and this backfill both key on `_id` and agree.
 *
 * ⚠️ It PRUNES, so it must read the corpus exactly as the serving path does. Showtimes
 * and slots live in the `screenings` / `movie_slots` side collections, and
 * `MongoMovieRepository` only stitches them back when those repositories are WIRED (they
 * default to `None` — the shape a non-serving maintenance script wants). Reading
 * unstitched here is what wiped live `web_screenings` on 2026-08-10: every row came back
 * with no showtimes, so the projection produced no screenings and the prune deleted the
 * lot. [[run]] now refuses to prune screenings when it projected none, and `main` wires
 * both side repositories.
 *
 * Run against prod via the ssh tunnel (see reference_prod_mongo_access):
 *   . scripts/local-mirror/prod-tunnel.sh && ensure_prod_tunnel   # ssh forward to mongo-1
 *   set -a; source .env.local; set +a
 *   sbt "worker/Test/runMain scripts.BackfillReadModel"
 */
object BackfillReadModel {

  /** Project every `movies` row into the read model and prune derived documents no
   *  longer produced. Pure over the repository traits, so `BackfillReadModelSpec`
   *  exercises it with in-memory repos. Returns
   *  (moviesWritten, screeningsWritten, moviesPruned, screeningsPruned).
   *
   *  Projecting NOTHING is never a reason to delete EVERYTHING: an empty projection
   *  means the read failed or the corpus reader was mis-wired, not that the corpus
   *  emptied (`MovieRepository.findAll`'s own contract says the same about its own empty
   *  result).
   *
   *  Screenings are additionally gated PER FILM, because the read that caused the
   *  2026-08-10 wipe can also fail for SOME films only (one keyset page's side-collection
   *  read, a subset of slots) and a global emptiness check passes straight through that.
   *  The abstention is narrow, so it costs no reaping: a film STILL IN the corpus that
   *  projected no screenings is evidence about the READ, so its existing screenings are
   *  left alone; a film that projected some is authoritative for itself, so a cinema that
   *  stopped screening it is pruned; and a film that left the corpus entirely is a genuine
   *  orphan and still reaped. Only the read-failure signature — present but silent — is
   *  spared. No threshold to tune. */
  def run(movieRepository: MovieRepository, readModel: ReadModelReader & ReadModelWriter): (Int, Int, Int, Int) = {
    val projected = movieRepository.findAll().flatMap(ReadModelProjection.projectAll(_, titleNormalizer))
    projected.foreach { case (movie, screenings) =>
      readModel.upsertMovie(movie)
      screenings.foreach(readModel.upsertScreening)
    }
    val expectedMovieIds     = projected.map(_._1._id).toSet
    val expectedScreeningIds = projected.flatMap(_._2.map(_._id)).toSet
    val filmsWithProjectedScreenings = projected.collect { case (movie, s) if s.nonEmpty => movie._id }.toSet
    // A film present in the corpus but projecting NO screenings is the read-failure
    // signature — the one case a prune must not act on.
    def readLooksTruncatedFor(filmId: String): Boolean =
      expectedMovieIds.contains(filmId) && !filmsWithProjectedScreenings.contains(filmId)

    val staleMovies =
      if (expectedMovieIds.isEmpty) Seq.empty
      else readModel.findAllMovies().filterNot(m => expectedMovieIds.contains(m._id))
    staleMovies.foreach(m => readModel.deleteMovie(m._id))
    val staleScreenings =
      if (expectedMovieIds.isEmpty) Seq.empty
      else readModel.findAllScreenings().filter { s =>
        !expectedScreeningIds.contains(s._id) && !readLooksTruncatedFor(s.filmId)
      }
    staleScreenings.foreach(s => readModel.deleteScreening(s._id))

    (projected.size, expectedScreeningIds.size, staleMovies.size, staleScreenings.size)
  }

  /** The corpus reader this script prunes against — THE one construction, so a spec can
   *  assert the wiring that actually runs rather than restate it (a restatement passes
   *  just as happily while `main` reads unstitched, which is how the 2026-08-10 wipe got
   *  through a green suite). Both side collections are wired: showtimes live in
   *  `screenings` and slots in `movie_slots`, and `MongoMovieRepository` stitches them
   *  back only when handed the repositories that own them.
   *  `BackfillReadModelStitchIntegrationSpec` drives this against a real Mongo. */
  def corpusReader(db: org.mongodb.scala.MongoDatabase): MovieRepository =
    new MongoMovieRepository(
      Some(db), fallbackToOwnInit = false, normalizer = titleNormalizer,
      screenings = Some(new MongoScreeningsRepository(Some(db))),
      slots      = Some(new MongoSlotsRepository(Some(db)))
    )

  def main(args: Array[String]): Unit = {
    val uri = Env.get("MONGODB_URI").getOrElse {
      println("MONGODB_URI not set."); sys.exit(1)
    }
    val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")
    val client = MongoClient(uri)
    try {
      val db            = client.getDatabase(dbName)
      val movieRepository     = corpusReader(db)
      val readModelRepository = new MongoReadModelRepository(Some(db))
      require(movieRepository.enabled,     s"movies repository not enabled for $dbName")
      require(readModelRepository.enabled, s"read-model repository not enabled for $dbName")

      val started = System.nanoTime()
      println(s"@@ backfilling read model from $dbName.movies …")
      val (movies, screenings, prunedM, prunedS) = run(movieRepository, readModelRepository)
      val secs = (System.nanoTime() - started) / 1e9
      println(f"@@ done in $secs%.1fs — wrote web_movies=$movies, web_screenings=$screenings" +
              s"${if (prunedM + prunedS > 0) s" (pruned $prunedM movie + $prunedS screening stale document(s))" else ""}")
    } finally client.close()
  }
}

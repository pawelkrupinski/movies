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
 * Run against prod via the flyctl proxy (see reference_prod_mongo_access):
 *   flyctl proxy 27017:27017 --app kinowo-mongo &
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
   *  emptied (`MovieRepository.findAll`'s own contract says the same about its empty
   *  result). Each half guards independently — films and screenings come from different
   *  collections, so one can legitimately be empty while the other isn't. */
  def run(movieRepository: MovieRepository, readModel: ReadModelReader & ReadModelWriter): (Int, Int, Int, Int) = {
    val projected = movieRepository.findAll().flatMap(ReadModelProjection.projectAll(_, titleNormalizer))
    projected.foreach { case (movie, screenings) =>
      readModel.upsertMovie(movie)
      screenings.foreach(readModel.upsertScreening)
    }
    val expectedMovieIds     = projected.map(_._1._id).toSet
    val expectedScreeningIds = projected.flatMap(_._2.map(_._id)).toSet

    val staleMovies =
      if (expectedMovieIds.isEmpty) Seq.empty
      else readModel.findAllMovies().filterNot(m => expectedMovieIds.contains(m._id))
    staleMovies.foreach(m => readModel.deleteMovie(m._id))
    val staleScreenings =
      if (expectedScreeningIds.isEmpty) Seq.empty
      else readModel.findAllScreenings().filterNot(s => expectedScreeningIds.contains(s._id))
    staleScreenings.foreach(s => readModel.deleteScreening(s._id))

    (projected.size, expectedScreeningIds.size, staleMovies.size, staleScreenings.size)
  }

  def main(args: Array[String]): Unit = {
    val uri = Env.get("MONGODB_URI").getOrElse {
      println("MONGODB_URI not set."); sys.exit(1)
    }
    val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")
    val client = MongoClient(uri)
    try {
      val db            = client.getDatabase(dbName)
      // Both side collections MUST be wired: this script prunes, so it has to read the
      // stitched corpus the serving path reads. Unwired, every row comes back with no
      // showtimes and the prune deletes live `web_screenings` (see the class doc).
      val movieRepository     = new MongoMovieRepository(
        Some(db), fallbackToOwnInit = false, normalizer = titleNormalizer,
        screenings = Some(new MongoScreeningsRepository(Some(db))),
        slots      = Some(new MongoSlotsRepository(Some(db)))
      )
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

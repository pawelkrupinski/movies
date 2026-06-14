package scripts

import org.mongodb.scala.MongoClient
import services.movies.{MongoMovieRepository, MovieRepository}
import services.readmodel.{MongoReadModelRepository, ReadModelProjection, ReadModelReader, ReadModelWriter}
import tools.Env

/**
 * One-shot backfill: populate the denormalised read-model collections
 * (`web_movies`, `web_screenings`) from the existing `movies` collection, so the
 * serving app can read from them the moment it deploys — no empty-repertoire
 * window before the worker's first projection lands.
 *
 * A FULL idempotent reconcile: every `movies` row is projected and upserted,
 * then any derived doc not in the freshly-projected set is deleted (a film that
 * left the corpus, or a cinema that stopped screening one). Re-running converges
 * to the same state, and it's safe to run while the worker is live — the
 * projector's incremental writes and this backfill both key on `_id` and agree.
 *
 * Run against prod via the flyctl proxy (see reference_prod_mongo_access):
 *   flyctl proxy 27017:27017 --app kinowo-mongo &
 *   set -a; source .env.local; set +a
 *   sbt "worker/Test/runMain scripts.BackfillReadModel"
 */
object BackfillReadModel {

  /** Project every `movies` row into the read model and prune derived docs no
   *  longer produced. Pure over the repository traits, so `BackfillReadModelSpec`
   *  exercises it with in-memory repos. Returns
   *  (moviesWritten, screeningsWritten, moviesPruned, screeningsPruned). */
  def run(movieRepository: MovieRepository, readModel: ReadModelReader & ReadModelWriter): (Int, Int, Int, Int) = {
    val projected = movieRepository.findAll().map(ReadModelProjection.project)
    projected.foreach { case (movie, screenings) =>
      readModel.upsertMovie(movie)
      screenings.foreach(readModel.upsertScreening)
    }
    val expectedMovieIds     = projected.map(_._1._id).toSet
    val expectedScreeningIds = projected.flatMap(_._2.map(_._id)).toSet

    val staleMovies = readModel.findAllMovies().filterNot(m => expectedMovieIds.contains(m._id))
    staleMovies.foreach(m => readModel.deleteMovie(m._id))
    val staleScreenings = readModel.findAllScreenings().filterNot(s => expectedScreeningIds.contains(s._id))
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
      val movieRepository     = new MongoMovieRepository(Some(db), fallbackToOwnInit = false)
      val readModelRepository = new MongoReadModelRepository(Some(db))
      require(movieRepository.enabled,     s"movies repository not enabled for $dbName")
      require(readModelRepository.enabled, s"read-model repository not enabled for $dbName")

      val started = System.nanoTime()
      println(s"@@ backfilling read model from $dbName.movies …")
      val (movies, screenings, prunedM, prunedS) = run(movieRepository, readModelRepository)
      val secs = (System.nanoTime() - started) / 1e9
      println(f"@@ done in $secs%.1fs — wrote web_movies=$movies, web_screenings=$screenings" +
              s"${if (prunedM + prunedS > 0) s" (pruned $prunedM movie + $prunedS screening stale doc(s))" else ""}")
    } finally client.close()
  }
}

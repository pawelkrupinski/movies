package scripts

import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import services.movies.MongoMovieRepository
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * One-shot: drop every row from the `movies` collection so the next app
 * boot rebuilds from a clean slate.
 *
 * Use case: a structural change (e.g. the cleanTitle-strict tmdbId fold
 * gate) leaves existing rows in a shape the new code can't safely heal in
 * place. Rather than write a split-and-redistribute backfill, the worker's
 * first scrape pass repopulates the cache from scratch; the four `*Ratings`
 * services + the IMDb id resolver chain off the bus events and re-resolve
 * every row.
 *
 * Output (per CLAUDE.md): row count BEFORE → 0 AFTER, plus a small sample
 * of titles being dropped so the run is auditable in the terminal.
 *
 * Run: sbt "Test/runMain scripts.DropAllMovies"
 */
object DropAllMovies {

  def main(args: Array[String]): Unit = {
    val uri = Env.get("MONGODB_URI").getOrElse {
      println("MONGODB_URI not set — nothing to drop.")
      sys.exit(1)
    }
    val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")

    val repository   = new MongoMovieRepository()
    val before = repository.findAll()
    println(s"$dbName.movies: ${before.size} row(s) currently stored.\n")

    val Sample = 20
    before.take(Sample).foreach { r =>
      println(s"  DROP  '${r.title}' (${r.year.map(_.toString).getOrElse("?")})  " +
              s"tmdbId=${r.record.tmdbId.getOrElse("—")}  " +
              s"imdbId=${r.record.imdbId.getOrElse("—")}")
    }
    if (before.size > Sample) println(s"  (+ ${before.size - Sample} more)")
    repository.close()

    // `deleteMany({})` rather than `drop()` — drop would also remove any
    // indexes we'd configured outside this codebase; deleteMany keeps the
    // collection structure intact, just empties it.
    val client = MongoClient(uri)
    try {
      val coll    = client.getDatabase(dbName).getCollection("movies")
      val deleted = Await.result(coll.deleteMany(org.mongodb.scala.bson.collection.immutable.Document()).toFuture(), 60.seconds)
      println()
      println("════ Summary ════")
      println(s"  Deleted: ${deleted.getDeletedCount} row(s)")
      println(s"  Remaining: ${Await.result(coll.countDocuments().toFuture(), 30.seconds)}")
      println()
      println("Next app boot: the worker's first scrape repopulates the cache;")
      println("each new MovieDetailsComplete event drives a fresh TMDB/IMDb/MC/RT/Filmweb")
      println("resolution. Expect ~5 minutes of in-flight rating discovery before")
      println("the cache reaches steady state.")
    } finally client.close()
  }
}

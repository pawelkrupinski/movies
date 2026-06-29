package scripts

import services.enrichment.OMDbClient
import services.movies.{MongoMovieRepository, StoredMovieRecord}
import tools.{DaemonExecutors, RealHttpFetch}

import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContextExecutorService, Future}

/**
 * One-shot OMDb IDENTIFIER backfill: for every Mongo row missing `imdbId` or
 * `rottenTomatoesUrl`, recover them from OMDb (imdbId by title+year search,
 * rottenTomatoesUrl from OMDb's `tomatoURL`) and upsert — writing ONLY the
 * missing identifier, never a rating value, never overriding an existing one.
 * The canonical refreshers (`ImdbRatings` / `RottenTomatoesRatings`) then fetch
 * `imdbRating` / `rottenTomatoes` from the new ids/links on their next tick.
 *
 * Operates DIRECTLY on the repository (not the `MovieCache`) so it can't trigger
 * `rehydrate`'s mis-keyed-orphan reap (which would `deleteById` prod rows).
 * Feature-gated on `OMDB_API_KEY`; a no-op when unset. Safe to re-run.
 *
 * Run: OMDB_API_KEY=… MONGODB_URI=… sbt "worker/Test/runMain scripts.OmdbBackfillRun"
 */
object OmdbBackfillRun {
  def main(args: Array[String]): Unit = {
    val repository = new MongoMovieRepository()
    if (!repository.enabled) {
      println("MONGODB_URI not set — nothing to backfill.")
      sys.exit(1)
    }
    if (OMDbClient.ApiKey.isEmpty) {
      println("OMDB_API_KEY not set — OMDb backfill is off.")
      sys.exit(1)
    }
    val omdb = new OMDbClient(new RealHttpFetch)

    val rows       = repository.findAll().sortBy(r => (r.title.toLowerCase, r.year))
    val candidates = rows.filter(r => r.record.imdbId.isEmpty || r.record.rottenTomatoesUrl.isEmpty)
    println(s"${rows.size} rows · ${candidates.size} missing imdbId or rottenTomatoesUrl · probing OMDb (8 workers)…\n")

    implicit val ec: ExecutionContextExecutorService = DaemonExecutors.boundedEC("omdb-backfill", 8)
    val idFills  = new AtomicInteger(0)
    val urlFills = new AtomicInteger(0)

    val tasks = candidates.map { case StoredMovieRecord(title, year, e, _) =>
      Future {
        // imdbId by title search (original/English title first — OMDb is an English DB).
        val foundId  = if (e.imdbId.isEmpty) omdb.findImdbId((e.originalTitle.toSeq :+ title).distinct, year) else None
        val effId    = e.imdbId.orElse(foundId)
        // rottenTomatoesUrl via the id we have or just recovered.
        val foundUrl = if (e.rottenTomatoesUrl.isEmpty) effId.flatMap(omdb.rottenTomatoesUrl) else None
        if (foundId.isDefined || foundUrl.isDefined) {
          repository.upsert(title, year, e.copy(
            imdbId            = e.imdbId.orElse(foundId),
            rottenTomatoesUrl = e.rottenTomatoesUrl.orElse(foundUrl)))
          foundId.foreach { id => idFills.incrementAndGet();  println(f"  imdbId  $title (${year.getOrElse("?")}) → $id") }
          foundUrl.foreach { u  => urlFills.incrementAndGet(); println(f"  RT-link $title (${year.getOrElse("?")}) → $u") }
        }
      }
    }
    Await.result(Future.sequence(tasks), 30.minutes)
    ec.shutdown()
    repository.close()

    println(s"\nDone. imdbId recovered: ${idFills.get} · rottenTomatoesUrl recovered: ${urlFills.get}.")
    println("Canonical ImdbRatings / RottenTomatoesRatings will fill the rating values from these on their next tick.")
  }
}

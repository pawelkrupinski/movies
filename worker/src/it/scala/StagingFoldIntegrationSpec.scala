package integration

import models.{Multikino, SourceData}
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoClient, SingleObservableFuture, ObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.MongoConnection
import services.movies.{MongoScreeningsRepository, MongoSlotsRepository, StoredMovieRecord}
import services.staging.{MongoStagingFolder, StagingRepository}
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * `MongoStagingFolder` against a real replica set — it needs transactions, so this is the
 * only layer that reaches it at all.
 *
 * The case under test is the one the unit specs structurally cannot see, and it is the
 * one that cost prod: the fold retires a `movies` row with a direct in-transaction
 * `deleteOne`, and most of those retirements are RE-KEYS (`foo|` collapsing onto
 * `foo|2026` once TMDB concludes the year), not films leaving. The film's showtimes are
 * still stored under the OLD id at that moment — the winner's side rows are written later,
 * by `MovieRepository.upsert` — so a fold that "tidies up" the loser's screenings destroys
 * them. Shipped @8033e39c6, it took PL from 39,413 upcoming showtimes to 18,161 and UK
 * from 22,250 to 7,226 within twenty minutes of deploy, hitting hardest exactly where the
 * most folding was happening.
 */
class StagingFoldIntegrationSpec extends AnyFlatSpec with Matchers {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val uri    = Env.get("MONGODB_URI").get
  private val dbName = Env.get("MONGODB_DB").getOrElse("kinowo")

  // Two year-variants of one film. `planGroup` collapses them onto the TMDB year, so the
  // other is a merge loser — deleted in-transaction, exactly the bypass under test.
  //
  // The name deliberately does NOT sanitize to an `integrationtest…` prefix, and that is
  // load-bearing: `MovieRepositoryIntegrationSpec` purges `^integrationtest` from `movies`
  // in its own beforeAll/afterAll, and sbt runs the it suites in PARALLEL against the one
  // `kinowo_it` database. A conventionally-named sentinel here passed alone and failed
  // alongside that spec — its rows deleted mid-fold by the neighbour's purge. Anything new
  // sharing this database wants a prefix of its own, and its own cleanup.
  private val title  = "__foldorphans-it-sentinel__"
  private val winner = StoredMovieRecord.idFor(title, Some(2026))
  private val loser  = StoredMovieRecord.idFor(title, Some(2025))

  private def sd(t: String) = SourceData(title = Some(t))

  it should "keep a retired key's screenings — the winner has not inherited them yet" in {
    val client     = MongoClient(uri)
    val db         = client.getDatabase(dbName)
    val connection = new MongoConnection(Some(uri), dbName, required = false)
    val slots      = new MongoSlotsRepository(Some(db))
    val screenings = new MongoScreeningsRepository(Some(db))
    val staging    = db.getCollection(StagingRepository.Collection)
    val movies     = db.getCollection(services.movies.MovieRepository.Collection)
    try {
      // Both year-variants exist in `movies`, each with cinemas in the side collections.
      Seq(winner, loser).foreach { id =>
        Await.result(movies.replaceOne(Filters.eq("_id", id),
          org.mongodb.scala.Document("_id" -> id, "tmdbId" -> 4242, "sourceData" -> org.mongodb.scala.Document(),
            "updatedAt" -> java.util.Date.from(java.time.Instant.now())),
          new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), 10.seconds)
        slots.replaceFilm(id, Map(Multikino.displayName -> sd("cinema")))
        screenings.replaceFilm(id, Map(Multikino.displayName -> Seq(
          models.Showtime(java.time.LocalDateTime.of(2026, 8, 1, 20, 0), None))))
      }
      slots.findForFilm(loser)      should not be empty
      screenings.findForFilm(loser) should not be empty

      // A staging row for the same group, concluded on the winner's year, drives the fold.
      val stagingId = s"${Multikino.displayName}|${services.movies.TitleNormalizer.sanitize(title)}|2026"
      Await.result(staging.replaceOne(Filters.eq("_id", stagingId),
        org.mongodb.scala.Document("_id" -> stagingId, "tmdbId" -> 4242,
          "sourceData" -> org.mongodb.scala.Document(Multikino.displayName ->
            org.mongodb.scala.Document("title" -> title)),
          "updatedAt" -> java.util.Date.from(java.time.Instant.now())),
        new com.mongodb.client.model.ReplaceOptions().upsert(true)).toFuture(), 10.seconds)

      new MongoStagingFolder(connection).foldGroup(title)

      val survivors = Await.result(movies.find(Filters.regex("_id",
        s"^${services.movies.TitleNormalizer.sanitize(title)}\\|")).toFuture(), 10.seconds)
        .flatMap(_.get("_id").map(_.asString().getValue))
      survivors      should not be empty   // premise: the fold did run
      survivors.size shouldBe 1            // …and did collapse the two into one

      // The retired key's cinemas must SURVIVE the fold. The winner's side rows are not
      // written by this transaction, so until `MovieRepository.upsert` next writes that
      // film these rows are the only copy of its showtimes — deleting them here is what
      // emptied prod. They become inert once the winner is written; `ReapOrphanedFilmRows`
      // clears them then, out of band, without racing the re-key.
      Seq(winner, loser).filterNot(survivors.contains).foreach { retired =>
        withClue(s"$retired was retired by the fold, but its showtimes are still the only copy: ") {
          screenings.findForFilm(retired) should not be empty
          slots.findForFilm(retired)      should not be empty
        }
      }
    } finally {
      Seq(winner, loser).foreach { id => slots.deleteFilm(id); screenings.deleteFilm(id) }
      Await.ready(movies.deleteMany(Filters.regex("_id",
        s"^${services.movies.TitleNormalizer.sanitize(title)}\\|")).toFuture(), 10.seconds)
      Await.ready(staging.deleteMany(Filters.regex("_id", s".*${services.movies.TitleNormalizer.sanitize(title)}.*"))
        .toFuture(), 10.seconds)
      client.close()
    }
  }
}

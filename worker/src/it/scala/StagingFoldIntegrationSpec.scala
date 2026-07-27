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
 * The case under test is the one the unit specs structurally cannot see: the fold deletes
 * a group-merge LOSER with a direct in-transaction `deleteOne` on `movies`, bypassing
 * `MovieRepository.delete` — which is what takes the film's cinemas in the side
 * collections with it. Nothing else ever cleans those up, so they outlived their film
 * forever: 888 `movie_slots` rows across 19 vanished films on prod PL 2026-07-27, plus 61
 * orphaned `screenings` films from the same bypass predating the slot split.
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

  it should "take a merge loser's slots and screenings with it when the fold deletes the film" in {
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

      // Whichever variant the collapse retired, its side rows must have gone with it —
      // a `movies` row that is gone must not leave cinemas behind. Fails before the fix:
      // the film vanished and its slots + screenings stayed forever.
      val survivors = Await.result(movies.find(Filters.regex("_id",
        s"^${services.movies.TitleNormalizer.sanitize(title)}\\|")).toFuture(), 10.seconds)
        .flatMap(_.get("_id").map(_.asString().getValue))
      Seq(winner, loser).filterNot(survivors.contains).foreach { gone =>
        withClue(s"$gone was deleted by the fold, so its side rows must be gone too: ") {
          slots.findForFilm(gone)      shouldBe empty
          screenings.findForFilm(gone) shouldBe empty
        }
      }
      survivors                     should not be empty   // premise: the fold did run
      survivors.size                shouldBe 1            // …and did collapse the two
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

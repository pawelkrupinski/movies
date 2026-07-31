package integration

import org.mongodb.scala.model.Filters
import org.mongodb.scala.{Document, MongoClient, ObservableFuture, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import models.{Multikino, MovieRecord, Showtime, Source, SourceData}
import services.staging.{MongoStagingRepository, StagingRecord}
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * The sibling lookup behind staging's duplicate-entry warning ranges over `_id` and uses
 * NOTHING but the `_id`s — yet it fetched and decoded every whole document in that
 * range, showtimes array and all.
 *
 * It runs on every FRESH insert, so its cost grows with the staged backlog, and a
 * convergence leg stages a whole country before folding any of it. That took `bootCorpus`
 * from 30 seconds against in-memory repositories to 3,360 against Mongo — a 113x
 * regression that timed the leg out at CI's ceiling — with the movie codec's `showtimes`
 * decoder the top frame in every JVM sample while Mongo itself was under 1% of wall
 * clock. The work was never the query; it was deserialising payloads to read one string
 * off each.
 *
 * Guarded on BYTES RETURNED rather than documents examined. `docsExamined` looked like
 * the obvious discriminator and quietly is not: for this query shape the server can
 * report 0 either way, which produced several confident-looking green runs that proved
 * nothing. `responseLength` cannot be faked — projected or not, the wire either carries
 * the payloads or it doesn't.
 *
 * Requires MONGODB_URI; skips otherwise.
 */
class StagingSiblingProjectionIntegrationSpec extends AnyFlatSpec with Matchers {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val client = MongoClient(Env.get("MONGODB_URI").get)
  private val db     = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
  private val staged = db.getCollection[Document]("pending_movies")

  private val title  = "Ghost In The Shell"
  /** Derived from `idFor`, so the seeded siblings share the prefix `upsert` will compute
   *  — a hand-written prefix silently ranges over nothing. */
  private val prefix = StagingRecord.idFor(Multikino, title, None).stripSuffix("")

  private def purge(): Unit =
    Await.result(staged.deleteMany(Filters.regex("_id", "^" + java.util.regex.Pattern.quote(prefix))).toFuture(), 30.seconds)

  /** Heavy staged rows written THROUGH THE REPOSITORY, so they are real
   *  `StoredMovieDto`s that decode back into `StagingRecord`s — hand-built documents
   *  measure bytes fine but cannot be read back, which silently turns a
   *  "these rows come back" assertion into a comparison against an empty list.
   *
   *  Heavy on purpose: the payload on the wire is the thing under test, and a light row
   *  cannot tell a projected read from an unprojected one. */
  private def seedHeavySiblings(repository: MongoStagingRepository): Unit = {
    val showtimes = (1 to 300).map(n =>
      Showtime(java.time.LocalDateTime.of(2026, 8, 1, 12, 0).plusMinutes(n.toLong * 7), Some(s"https://cinema.test/booking/$n")))
    Seq(1995, 2004, 2017).foreach(year =>
      repository.upsert(Multikino, title, Some(year),
        MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(
          title = Some(s"$title ($year)"), showtimes = showtimes)))))
  }

  "the staging sibling lookup" should "not pull the siblings' payloads over the wire" in {
    val repository = new MongoStagingRepository(Some(db))
    purge()
    try {
      seedHeavySiblings(repository)

      // The assertion is only meaningful if the rows are actually heavy. Without this a
      // future change that lightens the fixture would leave a test that passes whatever
      // the query does.
      val stored = Await.result(db.runCommand(Document("collStats" -> "pending_movies")).toFuture(), 30.seconds)
      val avgSize = stored.get("avgObjSize").map(_.asNumber().intValue()).getOrElse(0)
      withClue(s"fixture rows are only ${avgSize}B — too light for this assertion to mean anything: ") {
        avgSize should be > 3000
      }

      Await.result(db.runCommand(Document("profile" -> 0)).toFuture(), 30.seconds)
      Await.result(db.getCollection[Document]("system.profile").drop().toFuture(), 30.seconds)
      Await.result(db.runCommand(Document("profile" -> 2)).toFuture(), 30.seconds)
      // A FRESH id, so `upsert` takes the insert branch that runs the sibling lookup.
      try repository.upsert(Multikino, title, Some(2029), MovieRecord())
      finally Await.result(db.runCommand(Document("profile" -> 0)).toFuture(), 30.seconds)

      val ranged = Await.result(
        db.getCollection[Document]("system.profile")
          .find(Filters.and(Filters.eq("op", "query"), Filters.regex("ns", "pending_movies$")))
          .toFuture(), 30.seconds)
        .filter(_.get("nreturned").exists(_.asNumber().intValue() > 1))

      withClue("expected the sibling range query to be profiled: ") { ranged should not be empty }

      val bytes = ranged.flatMap(_.get("responseLength").map(_.asNumber().intValue())).max
      withClue(s"the sibling lookup pulled ${bytes}B back for a list of ids; projected to " +
               s"`_id` it returns a few hundred: ") {
        bytes should be < 2000
      }
    } finally purge()
  }

  // The reaper asks for ONE film's rows on every staging event. Filtering `findAll`
  // decoded every staged document to answer that — during an ingest that grows the
  // backlog, quadratically. `_id` already encodes the anchor (`cinema|anchor|year`), so
  // the rows can be identified from the index and only the matches fetched.
  "fetching one film's staging rows" should "not decode the whole collection" in {
    val repository = new MongoStagingRepository(Some(db))
    purge()
    try {
      seedHeavySiblings(repository)
      val anchor = StagingRecord.idFor(Multikino, title, None).split('|')(1)

      Await.result(db.runCommand(Document("profile" -> 0)).toFuture(), 30.seconds)
      Await.result(db.getCollection[Document]("system.profile").drop().toFuture(), 30.seconds)
      Await.result(db.runCommand(Document("profile" -> 2)).toFuture(), 30.seconds)
      val rows = try repository.findByAnchor(anchor)
                 finally Await.result(db.runCommand(Document("profile" -> 0)).toFuture(), 30.seconds)

      withClue("it must still return the film's rows: ") { rows.map(_.id).sorted shouldBe
        Seq(1995, 2004, 2017).map(y => s"$prefix$y").sorted }

      // The id sweep is covered by the `_id_` index; only the matching rows carry payload.
      // Scanning the collection would pull every document's showtimes instead.
      val queries = Await.result(
        db.getCollection[Document]("system.profile")
          .find(Filters.and(Filters.eq("op", "query"), Filters.regex("ns", "pending_movies$")))
          .toFuture(), 30.seconds)
      val sweep = queries.filter(_.get("nreturned").exists(_.asNumber().intValue() >= 3))
        .flatMap(_.get("responseLength").map(_.asNumber().intValue()))
      withClue(s"expected a profiled sweep over the collection, saw ${queries.size} query op(s): ") {
        sweep should not be empty
      }
      withClue(s"the id sweep pulled ${sweep.min}B; projected to `_id` it is a few hundred: ") {
        sweep.min should be < 2000
      }
    } finally purge()
  }
}

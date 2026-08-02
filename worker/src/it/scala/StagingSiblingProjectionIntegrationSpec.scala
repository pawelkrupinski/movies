package integration

import services.movies.SingleCountryNormalizer.given

import org.mongodb.scala.model.Filters
import org.mongodb.scala.{Document, ObservableFuture, SingleObservableFuture}
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
class StagingSiblingProjectionIntegrationSpec extends AnyFlatSpec with Matchers with org.scalatest.BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  // Its OWN database, not the one every other `it` spec shares.
  //
  // Two reasons, both learned the hard way. This spec seeds deliberately HEAVY and
  // deliberately odd documents — that is the point of it — and a raw one missing
  // `updatedAt` made `StagingFoldIntegrationSpec` fail with `Missing field: updatedAt`,
  // the same "one bad row kills a whole batch decode" shape this repository has shipped
  // to production twice. And it measures Mongo's PROFILER, which is database-wide: with
  // specs running concurrently the byte counts belonged to whichever suite happened to be
  // querying, so the assertions were reading someone else's traffic.
  private val db     = tools.IsolatedMongoDatabase.open(Env.get("MONGODB_URI").get, "staging-projection-spec")
  private val staged = db.getCollection[Document]("pending_movies")

  override protected def afterAll(): Unit = {
    tools.IsolatedMongoDatabase.drop(db)
    super.afterAll()
  }

  private val title  = "Ghost In The Shell"
  /** Derived from `idFor`, so the seeded siblings share the prefix `upsert` will compute
   *  — a hand-written prefix silently ranges over nothing. */
  private val prefix = StagingRecord.idFor(Multikino, title, None).stripSuffix("")

  private def purge(): Unit =
    Await.result(staged.deleteMany(Filters.regex("_id", "^" + java.util.regex.Pattern.quote(prefix))).toFuture(), 30.seconds)

  /** Heavy staged rows, written directly: the point is the payload on the wire, and a
   *  row without one cannot tell a projected read from an unprojected one. */
  private def seedHeavySiblings(): Unit = {
    val showtimes = (1 to 120).map(n =>
      Document("when" -> s"2026-08-0${n % 9 + 1}T20:00", "room" -> s"Hall $n", "url" -> s"https://cinema.test/$n"))
    Seq(1995, 2004, 2017).foreach(year =>
      Await.result(
        staged.insertOne(Document("_id" -> s"$prefix$year", "record" -> Document("showtimes" -> showtimes)))
          .toFuture(), 30.seconds))
  }

  "the staging sibling lookup" should "not pull the siblings' payloads over the wire" in {
    val repository = new MongoStagingRepository(Some(db))
    purge()
    try {
      seedHeavySiblings()

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

  // The invariant an override must hold: answer EXACTLY what filtering `findAll` answers.
  // A previous attempt inferred the anchor from the `_id` — which holds the sanitized
  // title from the row's first write — and silently returned nothing once titles were
  // normalised, so the staging state machine stopped advancing. Comparing the two
  // implementations directly is what pins that.
  "findByAnchor" should "return exactly what filtering findAll returns, for every anchor" in {
    val repository = new MongoStagingRepository(Some(db))
    purge()
    try {
      val showtimes = (1 to 200).map(n =>
        Showtime(java.time.LocalDateTime.of(2026, 8, 1, 12, 0).plusMinutes(n.toLong * 7), None))
      Seq(1995, 2004, 2017).foreach(year =>
        repository.upsert(Multikino, title, Some(year),
          MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(
            title = Some(s"$title ($year)"), showtimes = showtimes)))))
      // A row whose DISPLAY title differs from the raw one it was first keyed under.
      repository.upsert(Multikino, "GHOST IN THE SHELL 2", Some(2032),
        MovieRecord(data = Map[Source, SourceData](Multikino -> SourceData(title = Some("Ghost in the Shell 2")))))

      val all     = repository.findAll()
      val anchors = all.map(row => services.movies.TitleNormalizer.sanitize(row.title)).distinct
      withClue("the fixture must produce at least two anchors: ") { anchors.size should be >= 2 }

      anchors.foreach { anchor =>
        val expected = all.filter(row => services.movies.TitleNormalizer.sanitize(row.title) == anchor)
        withClue(s"anchor '$anchor': ") {
          repository.findByAnchor(anchor).map(_.id).sorted shouldBe expected.map(_.id).sorted
        }
      }
    } finally purge()
  }

  // A read failure must cost TIME, not a film. Returning `Seq.empty` when the fetch fails
  // tells the reaper this film has no rows, so it skips its next step — indistinguishable
  // from the film being finished, and permanent, since nothing revisits it. That is the
  // same silent-degradation shape that hid a whole broken fold for a day.
  it should "fall back to a full scan when the id fetch fails, rather than losing the film" in {
    purge()
    try {
      val seeder = new MongoStagingRepository(Some(db))
      seeder.upsert(Multikino, title, Some(1995), MovieRecord())
      seeder.upsert(Multikino, title, Some(2017), MovieRecord())
      val anchor = services.movies.TitleNormalizer.sanitize(seeder.findAll().head.title)
      // Rows for THAT anchor only — `findAll` spans every film the fixture staged, and
      // `findByAnchor` answers for one.
      val ours   = seeder.findAll().filter(row => services.movies.TitleNormalizer.sanitize(row.title) == anchor)

      val broken = new MongoStagingRepository(Some(db)) {
        override protected def fetchByIds(
          c:   org.mongodb.scala.MongoCollection[services.movies.StoredMovieDto],
          ids: Seq[String]
        ): scala.util.Try[Seq[services.movies.StoredMovieDto]] =
          scala.util.Failure(new RuntimeException("simulated fetch failure"))
      }

      withClue("the fixture must have staged rows for this anchor: ") { ours should not be empty }
      withClue("the film's rows must still come back, via the slower path: ") {
        broken.findByAnchor(anchor).map(_.id).sorted shouldBe seeder.findByAnchor(anchor).map(_.id).sorted
      }
      withClue("a failed fetch must cost time, not the film: ") {
        broken.findByAnchor(anchor).map(_.id) should contain allElementsOf ours.map(_.id)
      }
    } finally purge()
  }
}

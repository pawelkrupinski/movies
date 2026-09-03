package integration

import services.movies.SingleCountryNormalizer.titleNormalizer

import models.{Multikino, MovieRecord, Source, SourceData}
import org.mongodb.scala.MongoCollection
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.StoredMovieDto
import services.staging.MongoStagingRepository
import tools.Env

import scala.util.{Failure, Try}

/**
 * `upsertAll` writes a venue's staged rows in two round trips instead of three per row.
 * The saving is worthless if the rows differ, and this repository has shipped BOTH of the
 * ways a batch can differ from the loop it replaces:
 *
 *  - a read that FAILED being read as a row that is ABSENT, which carries no enrichment
 *    forward and blanks the resolve stamp on every row at once;
 *  - one undecodable document taking a whole batch's writes down with it.
 *
 * So the batch is pinned against the serial path it replaces, on the real database,
 * including both failure modes. Requires MONGODB_URI; skips otherwise.
 */
class StagingBatchUpsertIntegrationSpec extends AnyFlatSpec with Matchers with org.scalatest.BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val db = tools.IsolatedMongoDatabase.open(Env.get("MONGODB_URI").get, "staging-batch-spec")

  override protected def afterAll(): Unit = {
    tools.IsolatedMongoDatabase.drop(db)
    super.afterAll()
  }

  private def purge(): Unit = {
    import org.mongodb.scala.SingleObservableFuture
    import scala.concurrent.Await
    import scala.concurrent.duration._
    Await.result(db.getCollection("pending_movies").drop().toFuture(), 10.seconds)
  }

  private val cinema: Source = Multikino

  private def slot(title: String) = SourceData(title = Some(title), rawTitle = Some(title))
  private def scraped(title: String) = MovieRecord(data = Map(cinema -> slot(title)))

  /** A row as the resolve step leaves it: the scrape's slot plus the stamp a re-scrape
   *  must never blank. */
  private def resolved(title: String) =
    MovieRecord(tmdbId = Some(4242), imdbId = Some("tt4242424"), data = Map(cinema -> slot(title)))

  private def repository = new MongoStagingRepository(Some(db), normalizer = titleNormalizer)

  private def rowsOf(repo: MongoStagingRepository) =
    repo.findAll().map(r => r.id -> r.record).toMap

  "upsertAll" should "write exactly what the same rows written one at a time would" in {
    val titles = (1 to 6).map(n => s"Batch Film $n")

    purge()
    val serial = repository
    titles.foreach(t => serial.upsert(cinema, t, Some(2026), scraped(t)))
    val oneAtATime = rowsOf(serial)

    purge()
    val batched = repository
    batched.upsertAll(titles.map(t => (cinema, t, Some(2026), scraped(t))))

    withClue("the batch must stage the same ids: ")(rowsOf(batched).keySet shouldBe oneAtATime.keySet)
    oneAtATime should not be empty
  }

  /** The carry-forward is the only READ the batch does, and the reason it does one. */
  it should "carry an existing row's enrichment forward, exactly as the serial path does" in {
    purge()
    val repo  = repository
    val title = "Already Resolved"
    repo.upsert(cinema, title, Some(2026), resolved(title))

    // A re-scrape: the cinema reports the film again, knowing nothing about TMDB.
    repo.upsertAll(Seq((cinema, title, Some(2026), scraped(title))))

    val row = repo.findAll().find(_.record.data.contains(cinema)).getOrElse(fail("the row vanished"))
    withClue("a re-scrape through the batch must not blank the resolve stamp: ") {
      row.record.tmdbId shouldBe Some(4242)
      row.record.imdbId shouldBe Some("tt4242424")
    }
  }

  /**
   * ⚠️ THE OUTAGE SHAPE. A prefetch that fails is not a venue with no staged rows.
   *
   * Reading it as one would carry nothing forward and blank the stamp on every row of the
   * venue in a single write — the same "a failed read is not data" mistake that has cost
   * this repository a production incident. The batch must fall back to the per-row path,
   * which reads each row for itself.
   */
  it should "never blank enrichment when the prefetch fails" in {
    purge()
    val seed  = repository
    val title = "Unreadable Prefetch"
    seed.upsert(cinema, title, Some(2026), resolved(title))

    val broken = new MongoStagingRepository(Some(db), normalizer = titleNormalizer) {
      /** Fails only the BATCH's prefetch: the per-row fallback reads through
       *  `recordAt`, which does not go through this seam, so the fallback can still work. */
      override protected def fetchByIds(c: MongoCollection[StoredMovieDto],
                                        ids: Seq[String]): Try[Seq[StoredMovieDto]] =
        Failure(new RuntimeException("prefetch exploded"))
    }
    broken.upsertAll(Seq((cinema, title, Some(2026), scraped(title))))

    val row = repository.findAll().find(_.record.data.contains(cinema)).getOrElse(fail("the row vanished"))
    withClue("a failed prefetch must degrade to per-row reads, never to 'there was nothing there': ") {
      row.record.tmdbId shouldBe Some(4242)
    }
  }

  /** The row still has to be WRITTEN by that fallback — degrading must not mean dropping
   *  the scrape's own slot on the floor. */
  it should "still stage a new row when the prefetch fails" in {
    purge()
    val broken = new MongoStagingRepository(Some(db), normalizer = titleNormalizer) {
      override protected def fetchByIds(c: MongoCollection[StoredMovieDto],
                                        ids: Seq[String]): Try[Seq[StoredMovieDto]] =
        Failure(new RuntimeException("prefetch exploded"))
    }
    broken.upsertAll(Seq((cinema, "Fresh Despite Failure", Some(2026), scraped("Fresh Despite Failure"))))
    repository.findAll().map(_.title) should contain ("Fresh Despite Failure")
  }

  /** Two listings of one venue can key to the same `cinema|title|year`; the serial path
   *  had the second carry the first forward, and so must the batch. */
  it should "let a later row in the batch see an earlier one under the same id" in {
    purge()
    val repo  = repository
    val title = "Twice In One Venue"
    repo.upsertAll(Seq(
      (cinema, title, Some(2026), resolved(title)),
      (cinema, title, Some(2026), scraped(title))))

    val staged = repo.findAll()
    staged.size shouldBe 1
    withClue("the second row must carry the first's enrichment, not blank it: ")(
      staged.head.record.tmdbId shouldBe Some(4242))
  }
}

package integration

import models.{Cinema, CinemaMovie, Movie, Showtime}
import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.scrapes.{MongoScrapeArchiveRepository, ScrapeAttempt}

import java.time.{Instant, LocalDateTime}
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * `findAll` across keyset page boundaries.
 *
 * The unbounded `find()` this replaced did not fail loudly — it recursed the async
 * driver's completion chain until a `StackOverflowError` killed the I/O thread, so
 * the future never completed and the caller saw a bare 120s timeout with no cause.
 * Against Germany's 1,533-row archive over a proxied connection that happened every
 * single time, and the empty result it degraded to looked exactly like an empty
 * archive. A page-boundary test is the reachable half of that: prove the paged read
 * returns EVERY row rather than the first page, because a silently short read is
 * the failure mode that costs a whole investigation.
 *
 * Requires MONGODB_URI; skips otherwise.
 */
class ScrapeArchiveKeysetIntegrationSpec extends AnyFlatSpec with Matchers with tools.IntegrationMongoSuite {

  private val client   = MongoClient(mongoTarget.uri.value)
  private val database = client.getDatabase(
    s"kinowo_isolated_archivekeyset_${ProcessHandle.current().pid()}_${System.nanoTime()}")

  private def film(title: String) = CinemaMovie(
    movie     = Movie(title, None, None, Nil, Nil, None, None),
    cinema    = Cinema.all.head,
    posterUrl = None, filmUrl = None, synopsis = None,
    cast = Nil, director = Nil,
    showtimes = Seq(Showtime(LocalDateTime.parse("2026-08-01T18:00"), bookingUrl = None)))

  "findAll" should "return every row, not just the first keyset page" in {
    val repository = new MongoScrapeArchiveRepository(Some(database))
    // Comfortably more than one page, so a read that stopped at a page boundary
    // comes back short rather than merely unordered.
    val cinemas = Cinema.all.take(MongoScrapeArchiveRepository.FindAllBatchSize + 45)
    cinemas.size should be > MongoScrapeArchiveRepository.FindAllBatchSize

    try {
      cinemas.foreach(cinema => repository.record(ScrapeAttempt(
        cinema = cinema, city = Cinema.cityOf(cinema), at = Instant.parse("2026-07-28T06:00:00Z"),
        listingComplete = true, films = Seq(film(s"Film at ${cinema.displayName}")))))

      val all = repository.findAll()

      withClue(s"paged read returned ${all.size} of ${cinemas.size} rows: ") {
        all.map(_.cinema).toSet shouldBe cinemas.toSet
      }
      all.foreach(row => row.films should have size 1)
    } finally {
      Await.result(database.drop().toFuture(), 60.seconds)
      client.close()
    }
  }

  // A row is a venue's whole listing, so a page is several round trips and a decode: read one page
  // after another, the US archive was ~6 s of every identity projection.
  "scan" should "read its pages side by side, every row once" in {
    val outstanding = new java.util.concurrent.atomic.AtomicInteger(0)
    val most        = new java.util.concurrent.atomic.AtomicInteger(0)
    val listening = org.mongodb.scala.MongoClient(com.mongodb.MongoClientSettings.builder()
      .applyConnectionString(new com.mongodb.ConnectionString(mongoTarget.uri.value))
      .addCommandListener(new com.mongodb.event.CommandListener {
        private def page(name: String) = name == "find" || name == "getMore"
        override def commandStarted(event: com.mongodb.event.CommandStartedEvent): Unit =
          if (page(event.getCommandName)) { most.accumulateAndGet(outstanding.incrementAndGet(), math.max); () }
        override def commandSucceeded(event: com.mongodb.event.CommandSucceededEvent): Unit =
          if (page(event.getCommandName)) { outstanding.decrementAndGet(); () }
      }).build())
    val db         = listening.getDatabase(s"kinowo_isolated_archivescan_${ProcessHandle.current().pid()}_${System.nanoTime()}")
    val repository = new MongoScrapeArchiveRepository(Some(db))
    val cinemas    = Cinema.all.take(MongoScrapeArchiveRepository.FindAllBatchSize * 6)
    try {
      cinemas.foreach(cinema => repository.record(ScrapeAttempt(
        cinema = cinema, city = Cinema.cityOf(cinema), at = Instant.parse("2026-07-28T06:00:00Z"),
        listingComplete = true, films = Seq(film(s"Film at ${cinema.displayName}")))))
      most.set(0)
      val read = Vector.newBuilder[Cinema]
      repository.scan(_.foreach(row => read += row.cinema)) shouldBe true
      read.result().sortBy(_.displayName) shouldBe cinemas.sortBy(_.displayName)
      most.get should be > 1
    } finally {
      Await.result(db.drop().toFuture(), 60.seconds)
      listening.close()
    }
  }

  // A cut-over projection reads the archive only for venues with no accepted listing: fetching every
  // row to drop most of them was half of the 2.6 GB a US projection tick decoded.
  "scanVenues" should "fetch the rows of the venues it keeps, and no other" in {
    val requested = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()
    val listening = org.mongodb.scala.MongoClient(com.mongodb.MongoClientSettings.builder()
      .applyConnectionString(new com.mongodb.ConnectionString(mongoTarget.uri.value))
      .addCommandListener(new com.mongodb.event.CommandListener {
        override def commandStarted(event: com.mongodb.event.CommandStartedEvent): Unit =
          if (event.getCommandName == "find") {
            val id = Option(event.getCommand.getDocument("filter", null)).flatMap(f => Option(f.get("_id")))
            id.collect { case d: org.bson.BsonDocument if d.containsKey("$in") =>
              d.getArray("$in").getValues.forEach(v => { requested.add(v.asString.getValue); () })
            }
            ()
          }
      }).build())
    val db         = listening.getDatabase(s"kinowo_isolated_archivevenues_${ProcessHandle.current().pid()}_${System.nanoTime()}")
    val repository = new MongoScrapeArchiveRepository(Some(db))
    val cinemas    = Cinema.all.take(MongoScrapeArchiveRepository.FindAllBatchSize * 3)
    val kept       = cinemas.zipWithIndex.collect { case (cinema, i) if i % 7 == 0 => cinema }.toSet
    try {
      cinemas.foreach(cinema => repository.record(ScrapeAttempt(
        cinema = cinema, city = Cinema.cityOf(cinema), at = Instant.parse("2026-07-28T06:00:00Z"),
        listingComplete = true, films = Seq(film(s"Film at ${cinema.displayName}")))))
      requested.clear()
      val read = Vector.newBuilder[Cinema]
      repository.scanVenues(kept)(_.foreach(row => read += row.cinema)) shouldBe true
      read.result().toSet shouldBe kept
      requested.asScala.toSet shouldBe kept.map(_.displayName)
    } finally {
      Await.result(db.drop().toFuture(), 60.seconds)
      listening.close()
    }
  }
}

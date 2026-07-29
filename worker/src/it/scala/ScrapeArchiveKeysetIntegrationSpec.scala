package integration

import models.{Cinema, CinemaMovie, Movie, Showtime}
import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.scrapes.{MongoScrapeArchiveRepository, ScrapeAttempt}
import tools.Env

import java.time.{Instant, LocalDateTime}
import scala.concurrent.Await
import scala.concurrent.duration._

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
class ScrapeArchiveKeysetIntegrationSpec extends AnyFlatSpec with Matchers {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val client   = MongoClient(Env.get("MONGODB_URI").get)
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
}

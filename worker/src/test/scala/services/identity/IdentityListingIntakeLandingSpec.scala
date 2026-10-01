package services.identity

import models.{Cinema, CinemaMovie, Helios, Movie, Multikino, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{InMemoryScrapeGuardLedger, SingleCountryNormalizer}
import services.scrapes.{ArchivedScrape, InMemoryScrapeArchiveRepository}

import java.time.{Clock, Instant, LocalDateTime, ZoneOffset}
import java.util.concurrent.{CyclicBarrier, Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

/** A cut-over country's scrapes land through the intake one venue at a time per venue, never one at
 *  a time for the whole country — a US scrape walk is 4,462 venues of round-trips — and a scrape the
 *  intake keeps costs no second read of the listing it already holds. */
class IdentityListingIntakeLandingSpec extends AnyFlatSpec with Matchers {

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val clock      = Clock.fixed(Instant.parse("2026-09-26T10:00:00Z"), ZoneOffset.UTC)
  private val start      = LocalDateTime.of(2026, 9, 27, 18, 0)

  private def listing(cinema: Cinema, titles: String*): Seq[CinemaMovie] =
    titles.map(title => CinemaMovie(Movie(title), cinema, None, None, None, Nil, Nil, Seq(Showtime(start, None))))

  /** The accepted listings, `onFind` run before each read of one. */
  private final class WatchedArchive(onFind: Cinema => Unit) extends InMemoryScrapeArchiveRepository {
    override def find(cinema: Cinema): Option[ArchivedScrape] = { onFind(cinema); super.find(cinema) }
  }

  private def intake(accepted: InMemoryScrapeArchiveRepository) =
    new IdentityListingIntake(accepted, new InMemoryScrapeArchiveRepository, new InMemoryScrapeGuardLedger, normalizer, 3, clock)

  private def land(intake: IdentityListingIntake, cinema: Cinema, films: Seq[CinemaMovie]): Unit = {
    intake.recordCinemaScrape(cinema, films, listingIsComplete = true, sourceKey = None, viaFallback = false); ()
  }

  "scrapes of different venues" should "land side by side" in {
    val bothReading = new CyclicBarrier(2)
    val landing     = new java.util.concurrent.atomic.AtomicBoolean(true)
    // Each venue's landing waits, mid-read, for the other's: landed one at a time, the second never arrives.
    val accepted = new WatchedArchive(_ => if (landing.get) { bothReading.await(5, TimeUnit.SECONDS); () })
    val target   = intake(accepted)
    val pool     = Executors.newFixedThreadPool(2)
    try Seq(Multikino -> listing(Multikino, "Lalka"), Helios -> listing(Helios, "Diuna"))
      .map { case (cinema, films) => pool.submit[Unit](() => land(target, cinema, films)) }
      .foreach(_.get(10, TimeUnit.SECONDS))
    finally { pool.shutdownNow(); () }
    landing.set(false)
    target.listingOf(Multikino).map(_.movie.title) shouldBe Seq("Lalka")
    target.listingOf(Helios).map(_.movie.title) shouldBe Seq("Diuna")
  }

  "a scrape the intake keeps" should "read the venue's accepted listing once, and publish it" in {
    val reads     = new AtomicInteger()
    val published = new java.util.concurrent.ConcurrentLinkedQueue[Seq[String]]()
    val accepted  = new WatchedArchive(_ => { reads.incrementAndGet(); () })
    val target    = new IdentityListingIntake(accepted, new InMemoryScrapeArchiveRepository, new InMemoryScrapeGuardLedger,
      normalizer, 3, clock, published = (_, films) => { published.add(films.map(_.movie.title)); () })
    land(target, Multikino, listing(Multikino, "Lalka", "Diuna"))
    reads.set(0)
    land(target, Multikino, listing(Multikino, "Lalka", "Diuna"))
    reads.get shouldBe 1
    import scala.jdk.CollectionConverters._
    published.asScala.toSeq shouldBe Seq(Seq("Lalka", "Diuna"), Seq("Lalka", "Diuna"))
  }
}

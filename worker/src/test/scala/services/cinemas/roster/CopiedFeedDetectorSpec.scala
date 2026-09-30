package services.cinemas.roster

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{Cinema, CinemaMovie, Country, KinoMikro, MikroBronowice, Movie, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.PrometheusExposition
import services.scrapes.{ArchivedScrape, InMemoryScrapeArchiveRepository, ScrapeAttempt, SuccessfulScrape}

import java.time.{Instant, LocalDateTime}

/**
 * A venue's scraped feed that is ANOTHER venue's — told by the booking sessions it lists (the
 * Syracuse IN Pickwick listed the Park Ridge IL one's Veezi sessions, 216 of 216), never by a
 * programme alone, which chain-mates share while each books through its own site.
 */
class CopiedFeedDetectorSpec extends AnyFlatSpec with Matchers {

  private val venues = Country.Poland.cities.flatMap(_.cinemas).distinct.filterNot(Set[Cinema](KinoMikro, MikroBronowice))
  private val (pickwick, parkRidge, chainMate, unwatched) = (venues(0), venues(1), venues(2), venues(3))
  private val watched = Set(pickwick, parkRidge, chainMate, KinoMikro, MikroBronowice)

  private val start = LocalDateTime.of(2026, 10, 1, 12, 0)

  /** `films` listings of four showtimes each, every showtime booked through `host`/`site`'s own session. */
  private def feed(venue: Cinema, site: String, films: Int = 5, host: String = "ticketing.us.veezi.com"): Seq[CinemaMovie] =
    (1 to films).map { f =>
      CinemaMovie(Movie(s"Film $f"), venue, None, None, None, Nil, Nil,
        (1 to 4).map(s => Showtime(start.plusHours(f * 10L + s), Some(s"https://$host/purchase/$site-$f-$s?siteToken=$site"))))
    }

  private final class World(seedArchive: Seq[ArchivedScrape] = Nil) {
    val registry = new PrometheusRegistry()
    val detector = new CopiedFeedDetector(CopiedFeedDetector.gauge(registry), Country.Poland, watched)
    def gauge: Option[Double] =
      PrometheusExposition.sample(PrometheusExposition.render(registry), CopiedFeedDetector.Name, """country="pl"""")
  }

  "CopiedFeedDetector" should "flag a venue whose listings book through another venue's own sessions, whatever host serves them" in {
    val w = new World
    w.detector.venueScraped(parkRidge, feed(parkRidge, "parkridge", host = "ticketing.useast.veezi.com"))
    w.detector.venueScraped(pickwick, feed(pickwick, "parkridge"))   // Park Ridge's sessions, another mirror
    w.detector.copiedPairs shouldBe Set(Seq(pickwick, parkRidge).map(_.displayName).sorted match { case Seq(a, b) => (a, b) })
  }

  it should "not flag chain-mates running one grid, each booking through its own sessions" in {
    val w = new World
    w.detector.venueScraped(parkRidge, feed(parkRidge, "097"))
    w.detector.venueScraped(chainMate, feed(chainMate, "084"))       // same films, same times, own sessions
    w.detector.copiedPairs shouldBe empty
  }

  it should "not flag venues sharing one generic link on every showtime" in {
    def generic(venue: Cinema) = feed(venue, "x").map(film => film.copy(showtimes = film.showtimes.map(_.copy(bookingUrl = Some("https://flicks.us/movie/film")))))
    val w = new World
    w.detector.venueScraped(parkRidge, generic(parkRidge))
    w.detector.venueScraped(pickwick, generic(pickwick))
    w.detector.copiedPairs shouldBe empty
  }

  it should "not flag a venue sharing fewer than three of its listings, or under half of them" in {
    val w = new World
    w.detector.venueScraped(parkRidge, feed(parkRidge, "parkridge"))
    w.detector.venueScraped(pickwick, feed(pickwick, "parkridge", films = 2) ++ feed(pickwick, "pickwick", films = 5).drop(2))
    w.detector.copiedPairs shouldBe empty
  }

  it should "not flag a pair on the shared distinct-venue allowlist" in {
    DistinctVenuePairs.contains(KinoMikro, MikroBronowice) shouldBe true
    val w = new World
    w.detector.venueScraped(KinoMikro, feed(KinoMikro, "mikro"))
    w.detector.venueScraped(MikroBronowice, feed(MikroBronowice, "mikro"))
    w.detector.copiedPairs shouldBe empty
  }

  it should "clear the pair once the copying venue is scraped with its own feed again" in {
    val w = new World
    w.detector.venueScraped(parkRidge, feed(parkRidge, "parkridge"))
    w.detector.venueScraped(pickwick, feed(pickwick, "parkridge"))
    w.detector.copiedPairs should have size 1
    w.detector.venueScraped(pickwick, feed(pickwick, "pickwick"))
    w.detector.copiedPairs shouldBe empty
  }

  // Only the venues read through an upstream known to copy feeds are watched (KnownToCopy).
  it should "neither check nor index a venue it does not watch" in {
    val w = new World
    w.detector.venueScraped(unwatched, feed(unwatched, "parkridge"))
    w.detector.venueScraped(pickwick, feed(pickwick, "parkridge"))
    w.detector.copiedPairs shouldBe empty
  }

  it should "watch exactly the venues read through a client known to copy feeds" in {
    CopiedFeedDetector.KnownToCopy shouldBe Set("FlicksClient", "Bilety24OrganizerClient")
  }

  // A copy that predates the restart must be found again, from the archive, without the seed's
  // older listing undoing a scrape that landed after boot.
  "Its boot seed" should "find a copy already in the archive, and publish only once the archive was read whole" in {
    val archive = new InMemoryScrapeArchiveRepository
    val at      = Instant.parse("2026-09-30T10:00:00Z")
    archive.record(ScrapeAttempt(parkRidge, None, at, listingComplete = true, feed(parkRidge, "parkridge")))
    archive.record(ScrapeAttempt(pickwick, None, at, listingComplete = true, feed(pickwick, "parkridge")))
    val w = new World
    w.gauge shouldBe None                       // nothing published before the seed: a restart is no "zero"
    w.detector.seedFrom(archive)
    w.detector.copiedPairs should have size 1
    w.gauge shouldBe Some(1.0)
  }

  it should "not let a venue's archived listing undo a scrape that landed since boot" in {
    val archive = Seq(ArchivedScrape(pickwick, None, Some(SuccessfulScrape(Instant.EPOCH, listingComplete = true, feed(pickwick, "parkridge"))), None),
                      ArchivedScrape(parkRidge, None, Some(SuccessfulScrape(Instant.EPOCH, listingComplete = true, feed(parkRidge, "parkridge"))), None))
    val w = new World
    w.detector.venueScraped(pickwick, feed(pickwick, "pickwick"))   // fixed upstream since
    w.detector.seed(archive)
    w.detector.copiedPairs shouldBe empty
  }

  "CopiedFeedArchive" should "file every scrape as before and hand each landing to the detector" in {
    val underlying = new InMemoryScrapeArchiveRepository
    val w          = new World
    val archive    = new CopiedFeedArchive(underlying, w.detector)
    val at         = Instant.parse("2026-09-30T10:00:00Z")
    archive.record(ScrapeAttempt(parkRidge, None, at, listingComplete = true, feed(parkRidge, "parkridge")))
    archive.record(ScrapeAttempt(pickwick, None, at, listingComplete = true, feed(pickwick, "parkridge")))
    underlying.find(pickwick).flatMap(_.lastSuccess).map(_.films.size) shouldBe Some(5)
    w.detector.copiedPairs should have size 1
  }

  "DistinctVenuePairs" should "name only venues some roster still holds" in {
    DistinctVenuePairs.unresolved shouldBe empty
  }
}

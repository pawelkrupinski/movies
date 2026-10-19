package services.cinemas.roster

import tools.SpecTimeouts

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

  /** `films` listings of four showtimes each, every showtime booked through `host`/`site`'s own Veezi session. */
  private def feed(venue: Cinema, site: String, films: Int = 5, host: String = "ticketing.us.veezi.com"): Seq[CinemaMovie] =
    (1 to films).map { f =>
      CinemaMovie(Movie(s"Film $f"), venue, None, None, None, Nil, Nil,
        (1 to 4).map(s => Showtime(start.plusHours(f * 10L + s), Some(s"https://$host/purchase/$site-$f-$s?siteToken=$site"))))
    }

  private final class World(seedArchive: Seq[ArchivedScrape] = Nil) {
    val registry = new PrometheusRegistry()
    val detector = new CopiedFeedDetector(CopiedFeedDetector.gauge(registry), Country.Poland, watched)
    def gauge: Option[Double] =
      PrometheusExposition.render(registry).linesIterator.find(_.startsWith(s"${CopiedFeedDetector.Name}{country=\"pl\"}"))
        .map(_.trim.split("\\s+").last.toDouble)
  }

  "CopiedFeedDetector" should "flag a venue whose listings book through another venue's own sessions" in {
    val w = new World
    w.detector.venueScraped(parkRidge, feed(parkRidge, "parkridge"))
    w.detector.venueScraped(pickwick, feed(pickwick, "parkridge"))   // Park Ridge's very links
    w.detector.copiedPairs shouldBe Set(Seq(pickwick, parkRidge).map(_.displayName).sorted match { case Seq(a, b) => (a, b) })
  }

  // The Syracuse IN Pickwick, 2026-09-23: Flicks booked it through the Park Ridge IL Pickwick's Veezi
  // purchase ids served from another regional mirror -- ticketing.useast. vs ticketing.us. -- so the
  // links differ as strings but name one session. Keeping the whole host (2026-10-12) lost this pair.
  it should "flag a venue booking another's sessions through another regional mirror of one backend" in {
    val w = new World
    w.detector.venueScraped(parkRidge, feed(parkRidge, "parkridge", host = "ticketing.useast.veezi.com"))
    w.detector.venueScraped(pickwick, feed(pickwick, "parkridge"))
    w.detector.copiedPairs shouldBe Set(Seq(pickwick, parkRidge).map(_.displayName).sorted match { case Seq(a, b) => (a, b) })
  }

  // 2026-10-08..10: Reel Cinema Quinton and Borehamwood held DuplicateVenueListing (uk) for two days,
  // each "listing 6 of its 12 listings" through the other's sessions. Both book on admit-one, which
  // names the venue in the HOST (reelquinton. vs reelborehamwood.admit-one.co.uk) and numbers each
  // venue's performances on its own counter, so `/?p=seats&perfCode=20303` is a session at both
  // (8 perfCodes in common over the fortnight probed live 2026-10-12).
  it should "not flag two venues whose own booking hosts happen to number their sessions alike" in {
    def admitOne(venue: Cinema, tenant: String) = feed(venue, "x").zipWithIndex.map { case (film, f) =>
      film.copy(showtimes = film.showtimes.zipWithIndex.map { case (s, i) =>
        s.copy(bookingUrl = Some(s"https://$tenant.admit-one.co.uk/?p=seats&perfCode=${20300 + f * 4 + i}")) })
    }
    val w = new World
    w.detector.venueScraped(parkRidge, admitOne(parkRidge, "reelquinton"))
    w.detector.venueScraped(pickwick, admitOne(pickwick, "reelborehamwood"))
    w.detector.copiedPairs shouldBe empty
  }

  // Two independent venues on one ticketing product, each on its OWN domain, share the first label
  // (`www.`, `tickets.`) and the product's path, and number sessions on their own counters: keeping
  // only the first label read them as one venue listing the other's feed.
  it should "not flag two venues on different domains whose hosts share only the first label" in {
    def ownDomain(venue: Cinema, domain: String) = feed(venue, "x").zipWithIndex.map { case (film, f) =>
      film.copy(showtimes = film.showtimes.zipWithIndex.map { case (s, i) =>
        s.copy(bookingUrl = Some(s"https://www.$domain/Ticketing/visSelectTickets.aspx?txtSessionId=${1000 + f * 4 + i}")) })
    }
    val w = new World
    w.detector.venueScraped(parkRidge, ownDomain(parkRidge, "parkridgecinema.com"))
    w.detector.venueScraped(pickwick, ownDomain(pickwick, "pickwicktheatre.co.uk"))
    w.detector.copiedPairs shouldBe empty
  }

  "a session" should "keep the host's first label and its domain, dropping only a regional mirror label between them" in {
    CopiedFeedDetector.sessionOf("https://ticketing.useast.veezi.com/purchase/1") shouldBe "ticketing.veezi.com/purchase/1"
    CopiedFeedDetector.sessionOf("https://ticketing.us.veezi.com/purchase/1") shouldBe "ticketing.veezi.com/purchase/1"
    CopiedFeedDetector.sessionOf("https://reelquinton.admit-one.co.uk/?p=1") shouldBe "reelquinton.admit-one.co.uk/?p=1"
    CopiedFeedDetector.sessionOf("https://www.kino.pl/b?id=1") shouldBe "www.kino.pl/b?id=1"
    CopiedFeedDetector.sessionOf("https://host/b") shouldBe "host/b"
    CopiedFeedDetector.sessionOf("veezi-session-1") shouldBe "veezi-session-1"
  }

  "CopiedFeedDetector" should "not flag chain-mates running one grid, each booking through its own sessions" in {
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

  // A copy scraped clean took the shared sessions out of the index although the original still listed
  // them, so the next venue to copy the original went unflagged until the original was scraped again.
  it should "still flag a second copy of a venue whose first copy was scraped clean" in {
    val w = new World
    w.detector.venueScraped(parkRidge, feed(parkRidge, "parkridge"))
    w.detector.venueScraped(pickwick, feed(pickwick, "parkridge"))
    w.detector.venueScraped(pickwick, feed(pickwick, "pickwick"))
    w.detector.venueScraped(chainMate, feed(chainMate, "parkridge"))
    w.detector.copiedPairs shouldBe Set(Seq(chainMate, parkRidge).map(_.displayName).sorted match { case Seq(a, b) => (a, b) })
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
  // A US scrape walk lands eight venues at once; fingerprinting a listing reads only that listing.
  it should "fingerprint a landing venue's listings without waiting for another venue's landing" in {
    val w       = new World
    val read    = new java.util.concurrent.CountDownLatch(1)
    val films   = feed(pickwick, "pickwick")
    val watched = new scala.collection.immutable.AbstractSeq[CinemaMovie] {
      def iterator: Iterator[CinemaMovie] = { read.countDown(); films.iterator }
      def apply(i: Int): CinemaMovie      = films(i)
      def length: Int                     = films.length
    }
    val landing = new Thread(() => w.detector.venueScraped(pickwick, watched))
    w.detector.synchronized {                           // another venue's landing holds the detector
      landing.start()
      read.await(SpecTimeouts.Io.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS) shouldBe true
    }
    landing.join(SpecTimeouts.Io.toMillis)
    w.detector.copiedPairs shouldBe empty
  }

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

  /** The fingerprints as they were first computed — each listing's sessions cut out of their links,
   *  told apart as strings, each hashed by two `MurmurHash3.stringHash` passes — kept here as the
   *  reference the in-place computation must equal. */
  private def referenceFingerprints(films: Seq[CinemaMovie]): Array[Long] = {
    def hash64(session: String): Long =
      (scala.util.hashing.MurmurHash3.stringHash(session, 0x9747b28c).toLong << 32) |
        (scala.util.hashing.MurmurHash3.stringHash(session, 0x5bd1e995) & 0xffffffffL)
    films.iterator.flatMap { film =>
      val sessions = film.showtimes.iterator.flatMap(_.bookingUrl).map(CopiedFeedDetector.sessionOf).toSet
      Option.when(sessions.size >= CopiedFeedDetector.MinSessions)(sessions.iterator.map(hash64).min)
    }.toArray.distinct
  }

  "a venue's fingerprints" should "be exactly what cutting each session out of its link and hashing it gives" in {
    val rnd   = new scala.util.Random(20261001)
    val links = Seq("https://ticketing.us.veezi.com/purchase/", "http://useast.veezi.com/p/", "veezi-session-", "https://host.only",
      "https://host.only/", "https://h/a?b=c&d=", "ftp://x/y", "https://reelquinton.admit-one.co.uk/?p=seats&perfCode=",
      "https://h.x?q=", "https://a.b/c.d/", "https://www.a.co.uk/b?", "https://t.mirror.cdn.example.com/s/")
    val films = (1 to 400).map { f =>
      val showtimes = (0 until rnd.nextInt(9)).map { s =>
        val url = Option.when(rnd.nextInt(8) > 0)(links(rnd.nextInt(links.size)) + (if (rnd.nextBoolean()) s"$f-${rnd.nextInt(4)}" else "x" * rnd.nextInt(3)))
        Showtime(start.plusMinutes(s.toLong), url)
      }
      CinemaMovie(Movie(s"Film $f"), pickwick, None, None, None, Nil, Nil, showtimes)
    }
    films.grouped(7).foreach { venue =>
      CopiedFeedDetector.fingerprintsOf(venue).toSeq shouldBe referenceFingerprints(venue).toSeq
    }
    CopiedFeedDetector.fingerprintsOf(films).toSeq shouldBe referenceFingerprints(films).toSeq
    referenceFingerprints(films) should not be empty
  }
}

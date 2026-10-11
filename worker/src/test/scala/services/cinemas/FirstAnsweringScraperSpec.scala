package services.cinemas

import models.{CinemaMovie, Movie, Multikino, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.UptimeMonitor
import services.cinemas.ScriptedCinemaScraper.{NoShowtimes, OneMovie}
import services.cinemas.common.{CinemaScraper, FallbackAfter, FirstAnsweringScraper, ListingReads, SourceFallbackScraper}
import services.fallback.InMemoryFallbackStore

import java.time.{Instant, LocalDateTime}
import scala.util.Try

/** A German venue's fallback chain: Cineamo primary, then Filmstarts, kinoheld,
 *  kinoprogramm.com — each link tried in turn, the first with a showtime serving. */
class FirstAnsweringScraperSpec extends AnyFlatSpec with Matchers {

  private def boom(source: String) = new RuntimeException(s"$source down")

  private def listing(title: String): Seq[CinemaMovie] = Seq(CinemaMovie(Movie(title), Multikino, None, None, None, Nil, Nil,
    Seq(Showtime(LocalDateTime.parse("2026-10-12T20:00"), None))))

  private def source(outcome: Either[Throwable, Seq[CinemaMovie]]) = ScriptedCinemaScraper(Seq(outcome), repeatLast = true)

  "FirstAnsweringScraper" should "serve the first link that lists a showtime, never asking the ones after it" in {
    val filmstarts = source(Right(listing("Filmstarts")))
    val kinoheld   = source(Right(listing("kinoheld")))
    new FirstAnsweringScraper(Seq(filmstarts, kinoheld)).fetch() shouldBe listing("Filmstarts")
    kinoheld.calls shouldBe 0
  }

  it should "pass a venue on past a link that threw or listed nothing" in {
    val chain = new FirstAnsweringScraper(Seq(source(Left(boom("Filmstarts"))), source(Right(NoShowtimes)), source(Right(listing("kinoprogramm")))))
    chain.fetch() shouldBe listing("kinoprogramm")
  }

  it should "answer empty when every link that answered had nothing, and throw only when all of them threw" in {
    new FirstAnsweringScraper(Seq(source(Left(boom("Filmstarts"))), source(Right(Nil)))).fetch() shouldBe Nil
    the[RuntimeException] thrownBy new FirstAnsweringScraper(Seq(source(Left(boom("Filmstarts"))), source(Left(boom("kinoheld"))))).fetch() should
      have message "Filmstarts down"
  }

  it should "leave the listing incomplete only for the pages of the link that served" in {
    val passedOver = new CinemaScraper {
      val cinema = Multikino; def scrapeHosts = Set("a.test")
      def fetch(): Seq[CinemaMovie] = { ListingReads.pageFailed(boom("page 2")); Nil }
    }
    val (_, reads) = ListingReads.during(new FirstAnsweringScraper(Seq(passedOver, source(Right(OneMovie)))).fetch())
    reads.complete shouldBe true
  }

  // The runtime path the per-venue choice relies on: the primary (Cineamo) comes back
  // with nothing for a venue its fallbacks still list — three separate runs, as German
  // venues wait for — and the venue is served from the next source that has it.
  "A German venue's fallback chain" should "serve the next source once the primary has listed nothing for three runs" in {
    var now     = Instant.parse("2026-10-11T08:00:00Z")
    val cineamo = ScriptedCinemaScraper(Seq(Right(OneMovie), Right(Nil)), repeatLast = true)
    val chain   = new FirstAnsweringScraper(Seq(source(Left(boom("Filmstarts"))), source(Right(listing("kinoheld")))))
    val store   = new InMemoryFallbackStore
    val monitor = new UptimeMonitor(clock = java.time.Clock.fixed(now, java.time.ZoneOffset.UTC))
    val venue   = new SourceFallbackScraper(cineamo, fallback = () => Some(chain), fallbackName = "Filmstarts → kinoheld",
      fallbackRef = () => Some("A1489"), monitor, store, now = () => now, fallbackAfter = FallbackAfter.FailedRuns(3))
    def run() = { val served = Try(venue.fetchWithSource()); now = now.plusSeconds(3600); served.get }

    run().movies shouldBe OneMovie                  // healthy
    run().viaFallback shouldBe false                // empty once: riding it out
    run().viaFallback shouldBe false                // twice
    val third = run()
    third.viaFallback shouldBe true
    third.movies shouldBe listing("kinoheld")       // Filmstarts threw; kinoheld listed it
    store.get(Multikino.displayName).map(_.fallbackSource) shouldBe Some("Filmstarts → kinoheld")
  }
}

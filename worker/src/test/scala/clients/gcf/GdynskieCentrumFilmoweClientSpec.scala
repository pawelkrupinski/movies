package clients.gcf

import clients.tools.FakeHttpFetch
import models.GdynskieCentrumFilmowe
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.UptimeMonitor
import services.cinemas.{GdynskieCentrumFilmoweClient, RetryingCinemaScraper}
import tools.GetOnlyHttpFetch

import java.time.LocalDateTime
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._

/** Replays the recorded GCF repertoire page (07-06-2026 capture) through the
 *  client. The page is a server-rendered WordPress template at
 *  `https://gcf.org.pl/kino-studyjne/repertuar/`. Each `div.film-width` block
 *  holds one film; its `a.film-hours` anchors carry `data-date` + `data-hour`
 *  + booking URL — no per-film detail fetches needed. The fixture is at:
 *
 *    test/resources/fixtures/gcf/gcf.org.pl/kino-studyjne/repertuar/.html
 *
 *  Pinned concrete screenings seen live on 07-06-2026:
 *    "Diabeł ubiera się u Prady 2" on 2026-06-07 at 20:15
 */
class GdynskieCentrumFilmoweClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val http   = new FakeHttpFetch("gcf")
  private val client = new GdynskieCentrumFilmoweClient(http, GdynskieCentrumFilmowe)

  "GdynskieCentrumFilmoweClient" should "return a non-empty film list" in {
    val movies = client.fetch()
    movies should not be empty
  }

  it should "tag every film with GdynskieCentrumFilmowe" in {
    val movies = client.fetch()
    movies.map(_.cinema).toSet shouldBe Set(GdynskieCentrumFilmowe)
  }

  it should "give every film at least one showtime" in {
    val movies = client.fetch()
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening: Diabeł ubiera się u Prady 2 on 2026-06-07 at 20:15" in {
    val movies = client.fetch()
    val diabel = movies.find(_.movie.title == "Diabeł ubiera się u Prady 2").value
    diabel.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 7, 20, 15))
  }

  it should "include booking URLs pointing to bilet.gcf.org.pl" in {
    val movies = client.fetch()
    val bookings = movies.flatMap(_.showtimes).flatMap(_.bookingUrl)
    bookings.head should startWith("https://bilet.gcf.org.pl/MSI/")
  }

  // gcf.org.pl's WordPress origin flaps with frequent fast-fail HTTP 500s
  // (~10–25% of requests, in short bad windows). The cinema declares a raised
  // `maxFetchAttempts` so the production retry wrapper rides out a burst of them
  // within one tick instead of dropping GCF from the cache (a red uptime bar).
  // With the previous flat 3 attempts a 4-in-a-row burst gave up; the raised
  // budget recovers once the burst clears (replaying the real fixture).
  it should "survive a burst of transient 500s within its raised attempt budget" in {
    val failuresBeforeRecovery = 4
    val flaky = new GetOnlyHttpFetch {
      private val real  = new FakeHttpFetch("gcf")
      private val calls = new AtomicInteger(0)
      def get(url: String): String =
        if (calls.incrementAndGet() <= failuresBeforeRecovery)
          throw new RuntimeException(s"HTTP 500 for GET $url")
        else real.get(url)
    }
    val flakyClient = new GdynskieCentrumFilmoweClient(flaky, GdynskieCentrumFilmowe)
    flakyClient.maxFetchAttempts should be > failuresBeforeRecovery
    val scraper = new RetryingCinemaScraper(
      flakyClient,
      new UptimeMonitor(),
      maxAttempts    = flakyClient.maxFetchAttempts,
      initialBackoff = 1.millis
    )
    scraper.fetch() should not be empty
  }
}

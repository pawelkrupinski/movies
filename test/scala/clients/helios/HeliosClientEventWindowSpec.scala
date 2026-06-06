package clients.helios

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.HeliosClient
import tools.GetOnlyHttpFetch

import java.util.concurrent.CompletableFuture
import scala.collection.mutable

// The `/event` endpoint returns the cinema's ENTIRE event history when called
// without a date window — ~4400 events / 9 MB for Poznań, ~99% of them in the
// past — and downloading-then-discarding that payload every tick was the single
// dominant cost of a Helios scrape (~18-20s of a ~20s fetch). `/event` honours
// the same `dateTimeFrom`/`dateTimeTo` window as `/screening`, which trims it to
// the handful of in-window events (442ms / 45 KB). This pins that the event URL
// carries the window — and that it's the SAME window `/screening` uses, so the
// two enrichment sources stay aligned.
class HeliosClientEventWindowSpec extends AnyFlatSpec with Matchers {

  /** Records every URL the client requests and returns empty bodies, so we can
   *  assert on the request shape without a fixture. */
  private class RecordingFetch extends GetOnlyHttpFetch {
    val urls = mutable.ListBuffer[String]()
    override def get(url: String): String = { urls.synchronized(urls += url); "[]" }
    override def getAsync(url: String): CompletableFuture[String] =
      CompletableFuture.completedFuture(get(url))
  }

  "HeliosClient" should "request /event with the same date window as /screening" in {
    val fetch = new RecordingFetch
    new HeliosClient(fetch).fetch()

    val eventUrl     = fetch.urls.find(u => u.contains("/event")).getOrElse(
      fail(s"client never requested /event; saw: ${fetch.urls.mkString(", ")}"))
    val screeningUrl = fetch.urls.find(u => u.contains("/screening")).getOrElse(
      fail(s"client never requested /screening; saw: ${fetch.urls.mkString(", ")}"))

    val window = """dateTimeFrom=([^&]+)&dateTimeTo=([^&]+)""".r
    val eventWindow     = window.findFirstMatchIn(eventUrl)
    val screeningWindow = window.findFirstMatchIn(screeningUrl)

    withClue(s"event URL was: $eventUrl\n") { eventWindow shouldBe defined }
    // Same window on both — events and screenings enrich the same date range.
    eventWindow.map(_.matched) shouldBe screeningWindow.map(_.matched)
  }
}

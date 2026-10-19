package clients.helios

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import clients.tools.{ConstantHttpFetch, RequestLogHttpFetch}
import services.cinemas.pl.HeliosClient
import services.cinemas.common.ScrapeHorizon

import services.movies.SingleCountryNormalizer.titleNormalizer

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
  private def recordingFetch() = new RequestLogHttpFetch(new ConstantHttpFetch("[]"))

  "HeliosClient" should "request /event with the same date window as /screening" in {
    val fetch = recordingFetch()
    new HeliosClient(fetch, titles = titleNormalizer, today = _root_.tools.SpecClock.PinnedDay).fetch()

    val eventUrl     = fetch.gets.find(u => u.contains("/event")).getOrElse(
      fail(s"client never requested /event; saw: ${fetch.gets.mkString(", ")}"))
    val screeningUrl = fetch.gets.find(u => u.contains("/screening")).getOrElse(
      fail(s"client never requested /screening; saw: ${fetch.gets.mkString(", ")}"))

    val window = """dateTimeFrom=([^&]+)&dateTimeTo=([^&]+)""".r
    val eventWindow     = window.findFirstMatchIn(eventUrl)
    val screeningWindow = window.findFirstMatchIn(screeningUrl)

    withClue(s"event URL was: $eventUrl\n") { eventWindow shouldBe defined }
    // Same window on both — events and screenings enrich the same date range.
    eventWindow.map(_.matched) shouldBe screeningWindow.map(_.matched)
  }

  // The window used to END at today+6, and everything after it was invisible:
  // Poznań had 247 screenings inside those six days on 2026-08-05 and another 81
  // beyond them. Both endpoints take an arbitrary range, so the near week is
  // asked for as before and a second window sweeps the rest to the horizon.
  // `today` is by-name (the venue clock); read once per window build it could
  // straddle Warsaw midnight and leave the near and far windows a day apart —
  // a gap or an overlap in the programme. A clock that ticks a day per read
  // makes that straddle happen on every call.
  it should "build both windows from ONE reading of today" in {
    val fetch = recordingFetch()
    var day   = java.time.LocalDate.of(2026, 8, 5)
    def ticking: java.time.LocalDate = { val d = day; day = day.plusDays(1); d }
    new HeliosClient(fetch, today = ticking, titles = titleNormalizer).fetch()

    val windows = fetch.gets.filter(_.contains("/screening"))
      .flatMap("""dateTimeFrom=(\d{4}-\d{2}-\d{2})T[^&]*&dateTimeTo=(\d{4}-\d{2}-\d{2})""".r.findFirstMatchIn(_))
      .map(m => java.time.LocalDate.parse(m.group(1)) -> java.time.LocalDate.parse(m.group(2))).toSeq
    windows should have size 2
    windows(1)._1 shouldBe windows(0)._2.plusDays(1)
    windows(0)._2 shouldBe windows(0)._1.plusDays(6)
  }

  it should "sweep the programme past the near week, out to the scrape horizon" in {
    val fetch = recordingFetch()
    val today = java.time.LocalDate.of(2026, 8, 5)
    new HeliosClient(fetch, today = today, titles = titleNormalizer).fetch()

    def endsOf(endpoint: String): Seq[String] =
      fetch.gets.filter(_.contains(endpoint))
        .flatMap("""dateTimeTo=(\d{4}-\d{2}-\d{2})""".r.findFirstMatchIn(_).map(_.group(1))).toSeq

    val horizon = today.plusDays(ScrapeHorizon.MaxDays.toLong).toString
    // Both endpoints reach the horizon, and both still ask for the near week.
    endsOf("/screening") should contain allOf (today.plusDays(6).toString, horizon)
    endsOf("/event")     should contain allOf (today.plusDays(6).toString, horizon)
  }
}

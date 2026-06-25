package clients.kinoteka

import clients.tools.FakeHttpFetch
import models.{Kinoteka, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.{FilmDetail, KinotekaClient}
import tools.HttpFetch

import java.time.LocalDateTime
import java.util.concurrent.atomic.AtomicInteger

class KinotekaClientSpec extends AnyFlatSpec with Matchers {

  private val client  = new KinotekaClient(new FakeHttpFetch("kinoteka"))
  private val results = client.fetch()
  private val byTitle = results.map(cm => cm.movie.title -> cm).toMap

  private def detailFor(title: String): FilmDetail =
    client.fetchFilmDetail(
      byTitle(title).filmUrl.getOrElse(fail(s"no filmUrl for $title"))
    ).getOrElse(fail(s"no detail for $title"))

  "KinotekaClient.fetch" should "walk the date nav and return 82 films / 268 showtimes" in {
    results.size shouldBe 82
    results.flatMap(_.showtimes).size shouldBe 268
  }

  it should "assign Kinoteka to every entry" in {
    results.map(_.cinema).toSet shouldBe Set(Kinoteka)
  }

  it should "merge a film across days and carry listing fields on the bare fetch result" in {
    val m = byTitle("Zawodowcy")
    m.showtimes.size       shouldBe 28
    m.movie.genres         shouldBe Seq("Akcja", "Dramat")
    m.filmUrl              shouldBe Some("https://kinoteka.pl/film/zawodowcy/")
    byTitle("Diabeł ubiera się u Prady 2").showtimes.size shouldBe 21
  }

  it should "enrich runtime / year / countries / original title from the detail page" in {
    val d = detailFor("Zawodowcy")
    // The detail page lists two durations — "Czas trwania filmu" (the film) and
    // "Czas trwania reklam" (the trailer/ad block). Runtime must be the film's
    // 100 min, never the 15-min ad row.
    d.runtimeMinutes shouldBe Some(100)
    d.releaseYear    shouldBe Some(2026)
    d.countries      shouldBe Seq("USA", "Wielka Brytania")
    d.originalTitle  shouldBe Some("In the Grey")
  }

  it should "take the film poster from the hero <picture>, not the generic site og:image" in {
    detailFor("Zawodowcy").posterUrl shouldBe Some("https://medstore.kinoteka.pl/image001(1).jpg")
  }

  it should "read the cast list and the YouTube trailer off the detail page" in {
    val d = detailFor("Zawodowcy")
    d.cast       shouldBe Seq("Henry Cavill", "Rosamund Pike", "Jake Gyllenhaal")
    d.trailerUrl shouldBe Some("https://www.youtube.com/watch?v=AYq1ljpbNfA")
  }

  it should "read the film runtime even when the ad-block duration row comes first" in {
    val html =
      """<dl class="p-movie-details__general-info">
        |  <dt>Czas trwania reklam:</dt><dd>15 min</dd>
        |  <dt>Czas trwania filmu:</dt><dd>137 min</dd>
        |</dl>""".stripMargin
    KinotekaClient.parseDetail(html).runtime shouldBe Some(137)
  }

  it should "carry the screening booking URL with absolute date" in {
    byTitle("Zawodowcy").showtimes.head shouldBe
      Showtime(
        LocalDateTime.of(2026, 6, 5, 14, 15),
        Some("https://bilety.kinoteka.pl/#/screen?screeningId=42d45fbe-db10-4ef1-b5e4-0eeb0c149927&cinemaId=9ef78349-db9c-4dfc-85aa-96d030082c0d"),
        None, Nil
      )
  }

  // Regression: event pages append a "Harmonogram wydarzenia: …" agenda + a
  // partner/sponsor list as trailing body paragraphs; keep only the prose.
  it should "drop the trailing event agenda from synopses" in {
    val synopses = results.flatMap(_.filmUrl).flatMap(client.fetchFilmDetail).flatMap(_.synopsis)
    synopses.size should be >= 70 // the fix doesn't gut the corpus of synopses
    synopses.foreach(_ should not include "Harmonogram")
  }

  // Kinoteka's ~2 weeks of day pages used to be fetched ONE at a time, summing
  // to a ~30-70s scrape that pinned a worker slot and drained the shared-cpu
  // credit into a throttle. They're independent `?date=` GETs, so they now fetch
  // a few at a time — assert the listing/day fetches actually overlap.
  it should "fetch the date pages concurrently rather than one at a time" in {
    val probe = new KinotekaClientSpec.ConcurrencyProbeHttpFetch(new FakeHttpFetch("kinoteka"))
    new KinotekaClient(probe).fetch()
    probe.maxConcurrent should be > 1
  }
}

object KinotekaClientSpec {
  /** Wraps a fetch and tracks the peak number of `get`s in flight at once. Each
   *  call sleeps briefly so concurrent fetches actually overlap and are counted. */
  private class ConcurrencyProbeHttpFetch(inner: HttpFetch) extends HttpFetch {
    private val inFlight = new AtomicInteger(0)
    private val maxSeen  = new AtomicInteger(0)
    def maxConcurrent: Int = maxSeen.get

    override def get(url: String): String = {
      val now = inFlight.incrementAndGet()
      maxSeen.updateAndGet(m => math.max(m, now))
      try { Thread.sleep(40); inner.get(url) } finally inFlight.decrementAndGet()
    }

    override def post(url: String, body: String, contentType: String): String =
      inner.post(url, body, contentType)
  }
}

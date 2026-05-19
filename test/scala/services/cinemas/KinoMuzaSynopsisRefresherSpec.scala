package services.cinemas

import clients.tools.FakeHttpFetch
import models.{CinemaMovie, KinoMuza, Movie, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{CaffeineMovieCache, InMemoryMovieRepo}

import java.time.LocalDateTime

class KinoMuzaSynopsisRefresherSpec extends AnyFlatSpec with Matchers {

  private def cinemaMovie(title: String, filmUrl: String): CinemaMovie =
    CinemaMovie(
      movie     = Movie(title = title, releaseYear = Some(2026)),
      cinema    = KinoMuza,
      posterUrl = None,
      filmUrl   = Some(filmUrl),
      synopsis  = None,
      cast      = None,
      director  = None,
      showtimes = Seq(Showtime(LocalDateTime.of(2026, 5, 21, 18, 0), bookingUrl = None))
    )

  private def buildCache(films: Seq[CinemaMovie]): CaffeineMovieCache = {
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())
    cache.recordCinemaScrape(KinoMuza, films)
    cache
  }

  // ── Happy path: fetch a detail page that has a synopsis ───────────────────

  "refreshOne" should "fetch a Muza detail page and write the parsed synopsis into the slot" in {
    val cache     = buildCache(Seq(cinemaMovie("Pieniądze to wszystko", "https://www.kinomuza.pl/movie/pieniadze-to-wszystko/")))
    val refresher = new KinoMuzaSynopsisRefresher(cache, new KinoMuzaClient(new FakeHttpFetch("kino-muza")), new FakeHttpFetch("kino-muza"))

    refresher.refreshOne() shouldBe true

    val key = cache.keyOf("Pieniądze to wszystko", Some(2026))
    val slot = cache.get(key).get.data(KinoMuza)
    slot.synopsis                  should not be empty
    slot.synopsis.get              should not be empty
    slot.synopsis.get              should startWith ("James Cox Chambers Jr.")
  }

  // ── "Tried, nothing found" sentinel ───────────────────────────────────────
  //
  // When the detail page parses cleanly but the synopsis section is absent,
  // store Some("") so the next tick's candidate filter skips this row —
  // we don't want to re-fetch the same empty page forever.

  it should "save Some(\"\") when the detail page has no synopsis paragraph (sentinel for 'already tried')" in {
    // No `div.col-lg-7.paragraph` block → parseSynopsis returns None.
    val emptyHtml = "<html><body><div class='col-11 paragraph'><p>not the synopsis div</p></div></body></html>"
    val routingFetch = new FakeHttpFetch("kino-muza") {
      override def get(url: String): String = emptyHtml
    }
    val cache     = buildCache(Seq(cinemaMovie("Foo", "https://www.kinomuza.pl/movie/foo/")))
    val refresher = new KinoMuzaSynopsisRefresher(cache, new KinoMuzaClient(routingFetch), routingFetch)

    refresher.refreshOne() shouldBe true

    val slot = cache.get(cache.keyOf("Foo", Some(2026))).get.data(KinoMuza)
    slot.synopsis shouldBe Some("")
  }

  // After the sentinel is written, the next pass mustn't re-process the
  // same row — that's the whole point. The candidate filter has to treat
  // `None` and `Some("")` differently (None = "never tried", Some("") =
  // "tried, nothing").

  it should "not re-process a row whose Muza slot already carries the Some(\"\") sentinel" in {
    val routingFetch = new FakeHttpFetch("kino-muza") {
      override def get(url: String): String = throw new AssertionError(s"refreshOne should not have fetched $url")
    }
    val cache     = new CaffeineMovieCache(new InMemoryMovieRepo())
    // Seed the slot with the sentinel directly.
    val key = cache.keyOf("Foo", Some(2026))
    cache.put(key, models.MovieRecord(data = Map[Source, SourceData](
      KinoMuza -> SourceData(filmUrl = Some("https://www.kinomuza.pl/movie/foo/"), synopsis = Some(""))
    )))
    val refresher = new KinoMuzaSynopsisRefresher(cache, new KinoMuzaClient(routingFetch), routingFetch)

    refresher.refreshOne() shouldBe false
  }

  // ── Fetch failure: leave synopsis None so the next tick retries ──────────

  it should "leave synopsis as None when the HTTP fetch fails (retry next tick)" in {
    val failingFetch = new FakeHttpFetch("kino-muza") {
      override def get(url: String): String = throw new java.io.IOException("simulated fetch failure")
    }
    val cache     = buildCache(Seq(cinemaMovie("Foo", "https://www.kinomuza.pl/movie/foo/")))
    val refresher = new KinoMuzaSynopsisRefresher(cache, new KinoMuzaClient(failingFetch), failingFetch)

    // Returns true (a row was selected and attempted) even though the
    // synopsis didn't land — distinguishes "did work" from "found nothing
    // to do" for the scheduler-side tests.
    refresher.refreshOne() shouldBe true

    val slot = cache.get(cache.keyOf("Foo", Some(2026))).get.data(KinoMuza)
    slot.synopsis shouldBe None
  }

  // ── No-op when nothing needs refreshing ───────────────────────────────────

  it should "return false when no cached row needs a synopsis refresh" in {
    val routingFetch = new FakeHttpFetch("kino-muza") {
      override def get(url: String): String = throw new AssertionError(s"refreshOne should not have fetched $url")
    }
    val cache = new CaffeineMovieCache(new InMemoryMovieRepo())  // empty cache
    val refresher = new KinoMuzaSynopsisRefresher(cache, new KinoMuzaClient(routingFetch), routingFetch)

    refresher.refreshOne() shouldBe false
  }

  // ── Trailers ──────────────────────────────────────────────────────────────
  //
  // Muza's detail page embeds a YouTube `<iframe class="embed-responsive-item"
  // src="https://www.youtube.com/embed/<id>?…">`. The refresher runs the
  // same per-row fetch that pulls the synopsis and writes both fields back.

  it should "also extract and store the trailer URL on the same detail-page fetch" in {
    val cache     = buildCache(Seq(cinemaMovie("Dziecko z pyłu", "https://www.kinomuza.pl/movie/dziecko-z-pylu/")))
    val refresher = new KinoMuzaSynopsisRefresher(cache, new KinoMuzaClient(new FakeHttpFetch("kino-muza")), new FakeHttpFetch("kino-muza"))

    refresher.refreshOne() shouldBe true

    val slot = cache.get(cache.keyOf("Dziecko z pyłu", Some(2026))).get.data(KinoMuza)
    slot.trailerUrl shouldBe Some("https://www.youtube.com/watch?v=h9r7lx9yDXk")
    // Synopsis lands on the same tick, untouched.
    slot.synopsis   should not be empty
  }

  it should "leave the trailer slot None when the detail page has no embed iframe" in {
    val cache     = buildCache(Seq(cinemaMovie("Pieniądze to wszystko", "https://www.kinomuza.pl/movie/pieniadze-to-wszystko/")))
    val refresher = new KinoMuzaSynopsisRefresher(cache, new KinoMuzaClient(new FakeHttpFetch("kino-muza")), new FakeHttpFetch("kino-muza"))

    refresher.refreshOne() shouldBe true

    val slot = cache.get(cache.keyOf("Pieniądze to wszystko", Some(2026))).get.data(KinoMuza)
    slot.trailerUrl shouldBe None
  }

  // Upgrade the listing-page thumbnail with the detail page's portrait
  // poster. Same per-row fetch, three fields populated together.

  it should "upgrade the poster URL with the detail-page portrait variant" in {
    val cache     = buildCache(Seq(cinemaMovie("Pieniądze to wszystko", "https://www.kinomuza.pl/movie/pieniadze-to-wszystko/")))
    val refresher = new KinoMuzaSynopsisRefresher(cache, new KinoMuzaClient(new FakeHttpFetch("kino-muza")), new FakeHttpFetch("kino-muza"))

    refresher.refreshOne() shouldBe true

    val slot = cache.get(cache.keyOf("Pieniądze to wszystko", Some(2026))).get.data(KinoMuza)
    slot.posterUrl shouldBe Some("https://www.kinomuza.pl/content/uploads/2026/03/Pieniądze-to-wszystko-556x800.png")
  }

  it should "keep the existing posterUrl when the detail page has no portrait poster slot" in {
    // Pre-populate the Muza slot with a listing-page poster so we can
    // verify the refresher doesn't blow it away when the detail page
    // has no `img.img-fuild`. (`dziecko-z-pylu` happens to have one,
    // so this test would need its own fixture for a "no poster" case;
    // for now exercise the orElse fallback by mocking an empty HTML.)
    val cache = buildCache(Seq(cinemaMovie("Stub", "https://stub.test/")))
    cache.recordCinemaScrape(KinoMuza, Seq(cinemaMovie("Stub", "https://stub.test/")
      .copy(posterUrl = Some("https://listing-page-thumbnail.jpg"))))

    val emptyFetch = new tools.HttpFetch {
      override def get(url: String): String =
        "<html><body><img class='lazyload' data-src='https://stub.test/kadr.jpg'/></body></html>"
      override def post(url: String, body: String, contentType: String): String = ""
    }
    val refresher = new KinoMuzaSynopsisRefresher(cache, new KinoMuzaClient(emptyFetch), emptyFetch)
    refresher.refreshOne() shouldBe true

    val slot = cache.get(cache.keyOf("Stub", Some(2026))).get.data(KinoMuza)
    slot.posterUrl shouldBe Some("https://listing-page-thumbnail.jpg")  // unchanged
  }
}

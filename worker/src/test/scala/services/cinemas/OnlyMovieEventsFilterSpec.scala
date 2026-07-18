package services.cinemas

import clients.tools.FakeHttpFetch
import models._
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.CinemaScraper
import services.cinemas.pl.{BiletynaClient, NonMovieEventClassifier, OnlyMovieEventsFilter}

class OnlyMovieEventsFilterSpec extends AnyFlatSpec with Matchers with OptionValues {

  private def cm(title: String): CinemaMovie = CinemaMovie(
    movie = Movie(title = title), cinema = KinoRondo,
    posterUrl = None, filmUrl = None, synopsis = None, cast = Nil, director = Nil, showtimes = Nil
  )

  /** A client that mixes in the filter; `fetchUnfiltered` is its raw scrape. */
  private class StubClient(raw: Seq[CinemaMovie]) extends CinemaScraper with OnlyMovieEventsFilter {
    def cinema: Cinema = KinoRondo
    def scrapeHosts: Set[String] = Set("stub.test")
    protected def fetchUnfiltered(): Seq[CinemaMovie] = raw
  }

  "OnlyMovieEventsFilter" should "drop live events from fetch() but keep films and broadcasts" in {
    val client = new StubClient(Seq(
      cm("Dune: Part Two"),
      cm("Koncert Joscho Stephan Trio"),            // event → dropped
      cm("Kabaret Ani Mru Mru - Mniej więcej"),     // event → dropped
      cm("André Rieu W KINIE — letni koncert"),     // broadcast → kept
      cm("Avatar")
    ))
    client.fetch().map(_.movie.title) shouldBe
      Seq("Dune: Part Two", "André Rieu W KINIE — letni koncert", "Avatar")
  }

  it should "collapse an all-events tick to empty (recordCinemaScrape then keeps existing slots)" in {
    new StubClient(Seq(cm("Edyta Geppert - recital"), cm("Piotr Bałtroczyk Stand-up")))
      .fetch() shouldBe empty
  }

  // Proof the mix-in is wired on a REAL client: Kinoteatr Rondo's recorded
  // biletyna page lists the stage play "…Seks dla Opornych / Teatr Skene
  // Warszawa" alongside its films. BiletynaClient now mixes in the filter, so
  // fetch() must drop that event while keeping the film "Dyrygent".
  "BiletynaClient (with the mix-in)" should "filter the Kinoteatr Rondo stage event but keep its films" in {
    val titles = new BiletynaClient(
      new FakeHttpFetch("08-06-2026"), "https://biletyna.pl/Chelmno/Kinoteatr-Rondo", KinoRondo
    ).fetch().map(_.movie.title)

    titles should contain ("Dyrygent")
    titles.exists(_.toLowerCase.contains("teatr skene")) shouldBe false
    titles.exists(NonMovieEventClassifier.isLiveEvent) shouldBe false
  }
}

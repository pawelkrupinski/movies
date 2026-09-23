package clients.kzr_siedlec

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import models.KinoZaRogiemSiedlec
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.pl.KinoZaRogiemSiedlecClient

import java.time.{LocalDate, LocalDateTime}

/** Replays the recorded `goksiedlec.pl/kino/` WooCommerce product listing
 *  (2026-09-23 nearby-towns sweep, Zielona Góra catchment) through the
 *  client — one product per screening, spread across two paginated pages —
 *  proving the title/date/time baked into a single WooCommerce product title
 *  parses out correctly and that same-film screenings on different pages
 *  merge into one [[models.CinemaMovie]].
 *
 *  Previously unmodelled: the venue publishes only Facebook/poster listings
 *  for most of its sibling "Kino za Rogiem" network, but this one carries a
 *  real dated text schedule (see the 2026-09-23 coverage sweep notes). */
class KinoZaRogiemSiedlecClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies =
    new KinoZaRogiemSiedlecClient(new FakeHttpFetch("kzr-siedlec"), today = LocalDate.of(2026, 9, 23)).fetch()

  "KinoZaRogiemSiedlecClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoZaRogiemSiedlec)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening, parsed out of the baked-in product title" in {
    val film = movies.find(_.movie.title == "Pojedynek").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 9, 25, 17, 0))
  }

  it should "keep a nested opening quote inside the title (Andre Rieu) and merge its 3 screenings" in {
    val film = movies.find(_.movie.title.startsWith("Andre Rieu")).value
    film.movie.title shouldBe "Andre Rieu „Niech żyje Maastricht!"
    film.showtimes.map(_.dateTime) should contain theSameElementsAs Seq(
      LocalDateTime.of(2026, 10, 6, 17, 0),
      LocalDateTime.of(2026, 10, 15, 17, 0),
      LocalDateTime.of(2026, 10, 16, 17, 0)
    )
  }

  it should "walk pagination across both product-listing pages" in {
    // Page 1 carries 12 products, page 2 the remaining 9 (21 total, some
    // merged by title) — a client that stopped at page 1 would miss every
    // film only listed on page 2.
    movies.map(_.movie.title) should contain("Zwierzaki na zakręcie")
  }

  it should "resolve a poster from the lazy-loaded `data-src`, not the placeholder `src`" in {
    val film = movies.find(_.movie.title == "Pojedynek").value
    film.posterUrl.value should endWith("Plakat-1-300x225.jpg")
  }
}

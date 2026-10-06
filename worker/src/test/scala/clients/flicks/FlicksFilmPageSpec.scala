package clients.flicks

import clients.tools.{FixtureFile, ScriptedByUrlHttpFetch}
import models.{BarnCinemaDartingtonArtCentre, OdeonNorwich}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.{DetailFetchOutcome, FilmDetail, FlicksClient, FlicksFilmPage, FlicksMarket}

import java.time.LocalDate

/** Replays recorded Flicks film pages (`https://www.flicks.us/movie/<slug>/`, fetched 2026-10-06) through
 *  [[FlicksFilmPage.parse]]: the facts the day fragment lacks — above all the release year. */
class FlicksFilmPageSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val today = LocalDate.of(2026, 10, 6)

  private def page(slug: String): String = FixtureFile.read(s"test/resources/fixtures/flicks/www.flicks.us/movie/$slug.html")
  private def parsed(slug: String): FilmDetail = FlicksFilmPage.parse(page(slug), slug, today)

  "A Flicks film page" should "state the film's year, runtime, every director, country, cast and synopsis" in {
    val opera = parsed("a-night-at-the-opera")
    opera.releaseYear.value    shouldBe 1935
    opera.runtimeMinutes.value shouldBe 96
    // The day fragment's card renders only the first; the page credits both.
    opera.director             shouldBe Seq("Edmund Goulding", "Sam Wood")
    opera.countries            shouldBe Seq("USA")
    opera.cast.take(3)         shouldBe Seq("Groucho Marx", "Chico Marx", "Harpo Marx")
    opera.cast.size            shouldBe 10
    opera.genres               shouldBe Seq("Comedy")
    opera.synopsis.value       should startWith("The Marx Brothers take on high society")
  }

  it should "take the hero poster, never the og:image backdrop" in {
    val opera = parsed("a-night-at-the-opera")
    opera.posterUrl.value should include("/images/movies/poster/")
    opera.posterUrl.value should not include "/backdrop/"
  }

  it should "list each country of a co-production" in {
    parsed("pans-labyrinth").countries shouldBe Seq("Mexico", "Spain", "USA")
  }

  it should "state no original title: no Flicks page carries one" in {
    parsed("pans-labyrinth").originalTitle shouldBe None
  }

  it should "state no year for a re-release its slug bills as one and dates recently" in {
    // `/movie/shiva-re-release/` is Ram Gopal Varma's 1989 film, dated 2025 by its re-run.
    parsed("shiva-re-release").releaseYear shouldBe None
    parsed("shiva-re-release").runtimeMinutes.value shouldBe 161
  }

  it should "state no year for a re-release its title bills as one and dates recently" in {
    // "Crocodile Dundee: The Encore Cut" — the 1986 film, dated 2025.
    parsed("crocodile-dundee-the-encore-cut").releaseYear shouldBe None
  }

  it should "keep an old year whatever the billing: the page dates the film itself" in {
    parsed("lawrence-of-arabia-50th-anniversary-restoration").releaseYear.value shouldBe 1962
  }

  it should "read a page with nothing on it as an empty detail, not a failure" in {
    FlicksFilmPage.parse("<html><body></body></html>", "nothing", today) shouldBe FilmDetail()
  }

  "A Flicks venue" should "share one detail group with every venue of its market, and only of its market" in {
    def client(cinema: models.Cinema, market: FlicksMarket) =
      new FlicksClient(new ScriptedByUrlHttpFetch(_ => ""), "slug", cinema, market, detailHttp = new ScriptedByUrlHttpFetch(_ => ""), today = today)
    client(OdeonNorwich, FlicksMarket.UnitedKingdom).detailGroup shouldBe client(BarnCinemaDartingtonArtCentre, FlicksMarket.UnitedKingdom).detailGroup
    client(OdeonNorwich, FlicksMarket.UnitedKingdom).detailGroup should not be client(OdeonNorwich, FlicksMarket.UnitedStates).detailGroup
    client(OdeonNorwich, FlicksMarket.UnitedStates).pagesSharedAcrossVenues shouldBe true
  }

  it should "read a film's page through the detail fetch, not the listing's" in {
    val url    = "https://www.flicks.us/movie/a-night-at-the-opera/"
    val pages  = new ScriptedByUrlHttpFetch(u => if (u == url) page("a-night-at-the-opera") else throw new java.io.IOException(u))
    val client = new FlicksClient(new ScriptedByUrlHttpFetch(u => throw new java.io.IOException(s"listing fetch asked for $u")),
      "slug", OdeonNorwich, FlicksMarket.UnitedStates, detailHttp = pages, today = today)
    client.fetchDetail(url) match {
      case DetailFetchOutcome.Fetched(detail) => detail.releaseYear.value shouldBe 1935
      case other                              => fail(s"expected the page, got $other")
    }
  }
}

package clients.kino_diana

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import models.KinoDiana
import services.cinemas.pl.KinoDianaClient

import java.time.LocalDateTime

/** Replays the recorded biletykinodiana.pl eventCalendar feed through the client
 *  — proving it pulls the screening DATE from the JSON `date` field while mining
 *  the title, time and booking link out of the HTML blob inside each record's
 *  `title` string, and strips the trailing format tag ("- dubbing 2D").
 *
 *  Previously scraped from Filmweb, which had silently gone empty for it. */
class KinoDianaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoDianaClient(new FakeHttpFetch("kino-diana")).fetch()

  "KinoDianaClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoDiana)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a concrete screening — date from JSON, time from the HTML blob" in {
    val film = movies.find(_.movie.title.toLowerCase.contains("chłopiec na krańcach świata")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 19, 15, 30))
  }

  it should "strip the trailing format tag so screening variants merge" in {
    // Raw feed title: "CHŁOPIEC NA KRAŃCACH ŚWIATA - dubbing 2D".
    val titles = movies.map(_.movie.title)
    titles should contain("Chłopiec na krańcach świata")
    all(titles.map(_.toLowerCase)) should not include "dubbing"
  }

  it should "attach the /Bilety/Sala booking link to a screening" in {
    val film = movies.find(_.movie.title.toLowerCase.contains("chłopiec na krańcach świata")).value
    film.showtimes.flatMap(_.bookingUrl).exists(_.contains("/Bilety/Sala/")) shouldBe true
  }
}

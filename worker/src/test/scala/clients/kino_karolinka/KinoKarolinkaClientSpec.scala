package clients.kino_karolinka

import clients.tools.FakeHttpFetch
import models.KinoKarolinka
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.KinoKarolinkaClient

import java.time.LocalDateTime

/** Replays the recorded `karolinka.art.pl/wydarzenia` events list for Kino
 *  Karolinka (Lubliniec) — a mixed cultural venue — through the client, which
 *  must keep only the FILM screenings.
 *
 *  Previously scraped from Filmweb, which had silently gone empty for it. */
class KinoKarolinkaClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoKarolinkaClient(new FakeHttpFetch("kino-karolinka")).fetch()

  "KinoKarolinkaClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoKarolinka)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin a film screening and strip the '| Kino Karolinka' tag" in {
    val film = movies.find(_.movie.title == "Drzewo Magii").value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 20, 16, 0))
    movies.map(_.movie.title).foreach(_ should not include "Kino Karolinka")
  }

  it should "exclude non-film events (concerts, theatre, festivals)" in {
    // "XVIII Dni Radzionkowa" is a town festival, not a film.
    movies.map(_.movie.title.toLowerCase).exists(_.contains("dni radzionkowa")) shouldBe false
  }
}

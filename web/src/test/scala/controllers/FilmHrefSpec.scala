package controllers

import models.Poznan
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FilmHrefSpec extends AnyFlatSpec with Matchers {

  // The link is city-scoped; Poznań is the implicit city under test.
  private implicit val city: models.City = Poznan

  "FilmHref" should "produce a /{city}/film/{slug} URL" in {
    FilmHref("Belle") shouldBe "/poznan/film/belle"
  }

  it should "fold spaces, punctuation, and diacritics into the slug" in {
    // No percent-encoding survives: the slug is plain `a-z0-9-`, so the URL
    // needs no escaping and reads the same everywhere it's pasted.
    FilmHref("Mandalorian i Grogu") shouldBe "/poznan/film/mandalorian-i-grogu"
    FilmHref("Gwiezdne Wojny: Mandalorian i Grogu") shouldBe
      "/poznan/film/gwiezdne-wojny-mandalorian-i-grogu"
    FilmHref("Diabeł ubiera się u Prady 2") shouldBe
      "/poznan/film/diabel-ubiera-sie-u-prady-2"
  }

  it should "fall back to the query form when a title has no usable slug" in {
    // Nothing survives the fold, so there is no slug address to offer. The
    // query form still resolves (and `MovieController.film` renders it in place
    // rather than 301-ing to itself).
    FilmHref("!!!") shouldBe "/poznan/film?title=%21%21%21"
    FilmHref.slugOf("!!!") shouldBe None
    FilmHref.slugOf("Belle") shouldBe Some("belle")
  }

  it should "keep the og-image card on the query form" in {
    // The card is an asset, not an indexable page — leaving its URL alone means
    // the previews already cached by Facebook and friends don't all miss.
    FilmHref.ogImage("Mandalorian i Grogu") shouldBe
      "/poznan/film/og-image?title=Mandalorian%20i%20Grogu"
  }

  it should "still build the legacy query form for redirect targets" in {
    FilmHref.legacy("Diabeł ubiera się u Prady 2", Poznan) shouldBe
      "/poznan/film?title=Diabe%C5%82%20ubiera%20si%C4%99%20u%20Prady%202"
  }
}

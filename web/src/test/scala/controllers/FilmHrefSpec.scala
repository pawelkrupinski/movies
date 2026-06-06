package controllers

import models.Poznan
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FilmHrefSpec extends AnyFlatSpec with Matchers {

  // The link is city-scoped; Poznań is the implicit city under test.
  private implicit val city: models.City = Poznan

  "FilmHref" should "produce a /{city}/film URL with the title percent-encoded" in {
    FilmHref("Belle") shouldBe "/poznan/film?title=Belle"
  }

  it should "encode spaces, colons, and diacritics so the link round-trips" in {
    // Spaces become %20 (RFC 3986); colon → %3A; UTF-8 multi-byte
    // diacritics → their %xx %yy pair. We post-process the URLEncoder output
    // to swap `+` → `%20` because some link-preview scrapers (Facebook)
    // reject `+` for spaces as a malformed URL.
    FilmHref("Mandalorian i Grogu") shouldBe "/poznan/film?title=Mandalorian%20i%20Grogu"
    FilmHref("Gwiezdne Wojny: Mandalorian i Grogu") shouldBe
      "/poznan/film?title=Gwiezdne%20Wojny%3A%20Mandalorian%20i%20Grogu"
    FilmHref("Diabeł ubiera się u Prady 2") shouldBe
      "/poznan/film?title=Diabe%C5%82%20ubiera%20si%C4%99%20u%20Prady%202"
  }
}

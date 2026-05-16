package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FilmHrefSpec extends AnyFlatSpec with Matchers {

  "FilmHref" should "produce a /film URL with the title percent-encoded" in {
    FilmHref("Belle") shouldBe "/film?title=Belle"
  }

  it should "encode spaces, colons, and diacritics so the link round-trips" in {
    // Spaces become '+' (form-encoding); colon → %3A; UTF-8 multi-byte
    // diacritics → their %xx %yy pair.
    FilmHref("Mandalorian i Grogu") shouldBe "/film?title=Mandalorian+i+Grogu"
    FilmHref("Gwiezdne Wojny: Mandalorian i Grogu") shouldBe
      "/film?title=Gwiezdne+Wojny%3A+Mandalorian+i+Grogu"
    FilmHref("Diabeł ubiera się u Prady 2") shouldBe
      "/film?title=Diabe%C5%82+ubiera+si%C4%99+u+Prady+2"
  }
}

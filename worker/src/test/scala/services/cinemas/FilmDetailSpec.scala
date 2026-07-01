package services.cinemas

import models.{Showtime, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/** `FilmDetail.mergeInto` fills a cinema slot's gaps from the detail page. The
 *  language/format case matters for cinemas (e.g. Kino Paradox) that expose the
 *  version only on the detail page as a per-film field, never in the listing
 *  title: it must badge the film's showings without clobbering a per-screening
 *  format the listing already set. */
class FilmDetailSpec extends AnyFlatSpec with Matchers {

  private def at(h: Int, m: Int, book: String, fmt: List[String]) =
    Showtime(LocalDateTime.of(2026, 6, 7, h, m), Some(book), None, fmt)

  "FilmDetail.mergeInto" should "badge showings that lack a format with the detail-page language, keeping any listing format" in {
    val slot = SourceData(
      title     = Some("Chłopiec na krańcach świata"),
      showtimes = Seq(at(16, 45, "b1", Nil), at(19, 0, "b2", List("NAP")))
    )
    val merged = FilmDetail(format = List("LEK")).mergeInto(slot)
    // The un-badged showing gains LEK; the one the listing already set to NAP is left alone.
    merged.showtimes.map(_.format) shouldBe Seq(List("LEK"), List("NAP"))
  }

  it should "leave showings untouched when the detail carries no format" in {
    val slot = SourceData(title = Some("X"), showtimes = Seq(at(16, 45, "b", Nil)))
    FilmDetail(synopsis = Some("prose")).mergeInto(slot).showtimes.map(_.format) shouldBe Seq(Nil)
  }
}

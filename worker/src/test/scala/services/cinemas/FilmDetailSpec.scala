package services.cinemas

import services.movies.ScreeningTokens
import org.scalatest.matchers.should.Matchers
import models.{Showtime, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.common.FilmDetail

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
    val merged = FilmDetail(format = List("LEK")).mergeInto(slot, ScreeningTokens.Default)
    // The un-badged showing gains LEK; the one the listing already set to NAP is left alone.
    merged.showtimes.map(_.format) shouldBe Seq(List("LEK"), List("NAP"))
  }

  // Kino Pionier reused `pionier1907.pl/event/lalka` — the URL that had held
  // Wojciech Has's 1968 film — for the 2026 Maciej Kawalski one. `DetailReaper`
  // re-fetches that page every 6h and the page now plainly reads "Rok: 2026,
  // Czas trwania: 2h 42 min", but `mergeInto` fills only what is EMPTY, so the
  // 1968/151 captured from the old film could never be overwritten. That one
  // stale slot keyed a whole row `lalka|1968`, and 120 slots of the new film
  // piled onto it.
  "FilmDetail.refreshInto" should "replace detail-owned fields a re-fetch now contradicts" in {
    val stale = SourceData(
      title = Some("LALKA"), releaseYear = Some(1968), runtimeMinutes = Some(151),
      director = Seq("Wojciech Has"), synopsis = Some("stara ekranizacja"))
    val fresh = FilmDetail(
      releaseYear = Some(2026), runtimeMinutes = Some(162),
      director = Seq("Maciej Kawalski"), synopsis = Some("nowa ekranizacja"))

    val merged = fresh.refreshInto(stale, ScreeningTokens.Default)

    merged.releaseYear    shouldBe Some(2026)
    merged.runtimeMinutes shouldBe Some(162)
    merged.director       shouldBe Seq("Maciej Kawalski")
    merged.synopsis       shouldBe Some("nowa ekranizacja")
  }

  it should "keep what the LISTING owns, and what the re-fetch could not read" in {
    val slot = SourceData(
      title = Some("LALKA"), releaseYear = Some(1968), runtimeMinutes = Some(151),
      posterUrl = Some("https://pionier1907.pl/listing.jpg"),
      showtimes = Seq(at(20, 15, "b1", Nil)))
    // A detail page that parses nothing must not blank the slot.
    val merged = FilmDetail().refreshInto(slot, ScreeningTokens.Default)

    merged.title          shouldBe Some("LALKA")
    merged.posterUrl      shouldBe Some("https://pionier1907.pl/listing.jpg")
    merged.showtimes      should have size 1
    merged.releaseYear    shouldBe Some(1968)
    merged.runtimeMinutes shouldBe Some(151)
  }

  it should "leave showings untouched when the detail carries no format" in {
    val slot = SourceData(title = Some("X"), showtimes = Seq(at(16, 45, "b", Nil)))
    FilmDetail(synopsis = Some("prose")).mergeInto(slot, ScreeningTokens.Default).showtimes.map(_.format) shouldBe Seq(Nil)
  }
}

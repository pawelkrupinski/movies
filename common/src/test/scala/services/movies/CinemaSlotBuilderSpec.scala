package services.movies

import models.{CinemaMovie, Country, KinoApollo, Movie}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CinemaSlotBuilderSpec extends AnyFlatSpec with Matchers {

  private val slots = new CinemaSlotBuilder(Country.Poland.language, new StringPool)

  private def runtimeOf(minutes: Int, prior: Option[Int] = None): Option[Int] =
    slots.build(CinemaMovie(Movie("Film", runtimeMinutes = Some(minutes)), KinoApollo, None, None, None, Nil, Nil, Nil),
      "Film", prior.map(m => models.SourceData(runtimeMinutes = Some(m)))).runtimeMinutes

  // Filmtheater Bleicherode bills "flüstern & SCHREIEN" (1989) at 6000 minutes (recorded DE corpus, 2026-10-04).
  "CinemaSlotBuilder.build" should "read a runtime no screened film has as unpublished, like a zero" in {
    runtimeOf(6000) shouldBe None
    runtimeOf(0) shouldBe None
    runtimeOf(6000, prior = Some(98)) shouldBe Some(98)
    runtimeOf(432) shouldBe Some(432)
  }
}

class CinemaSlotBuilderFieldsSpec extends AnyFlatSpec with Matchers {

  private val slots = new CinemaSlotBuilder(Country.Poland.language, new StringPool)

  // Kino Roma Zabrze's poster (08-06-2026 fixture), Bilety24's home-page copy of a screening (Janosik,
  // 2026-10-01 corpus), a genre list read as one label (Kino Kolory), a detail-free country spelling.
  "CinemaSlotBuilder.build" should "land every field through SlotFields" in {
    val booked = models.Showtime(java.time.LocalDateTime.of(2026, 10, 12, 9, 0), Some("https://bilety24.pl/kino/moxy?id=983999"))
    val slot = slots.build(CinemaMovie(Movie("Moxy", countries = Seq("Niderlandy"), genres = Seq("Biograficzny/Muzyczny")), KinoApollo,
      posterUrl = Some("/app/assets/movie/zieS.jpg"), filmUrl = Some("https://www.kinoroma.zabrze.pl/film/moxy"), synopsis = None,
      cast = Nil, director = Nil, showtimes = Seq(booked, booked.copy(bookingUrl = Some("https://www.bilety24.pl#")))), "Moxy", None)
    slot.posterUrl shouldBe Some("https://www.kinoroma.zabrze.pl/app/assets/movie/zieS.jpg")
    slot.showtimes shouldBe Seq(booked)
    slot.genres shouldBe Seq("Biograficzny", "Muzyczny")
    slot.countries shouldBe Seq("Holandia")
    slot.filmUrl shouldBe Some("https://www.kinoroma.zabrze.pl/film/moxy")
  }
}

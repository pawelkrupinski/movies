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

class CinemaSlotBuilderCarrySpec extends AnyFlatSpec with Matchers {
  private val slots = new CinemaSlotBuilder(Country.Poland.language, new StringPool)
  private val prior = models.SourceData(title = Some("Lalka"), releaseYear = Some(2026), director = Seq("Maciej Kawalski"),
    synopsis = Some("Ekranizacja powieści"))

  // "Lalka 2D" at a venue that prints no page or credits took "Lalka"'s year and director from the slot it was built
  // over, keyed its slot as "Lalka", and was re-made a fresh film every projection (seed 703, prod retired-by-overlap).
  "CinemaSlotBuilder.build" should "key a page-less listing's slot as the listing, carrying no other listing's year or director" in {
    val pageless = CinemaMovie(Movie("Lalka 2D"), KinoApollo, None, None, None, Nil, Nil, Nil)
    val slot     = slots.build(pageless, "Lalka 2D", Some(prior))
    (slot.releaseYear, slot.director) shouldBe ((None, Nil))
    slot.synopsis shouldBe prior.synopsis                                 // what does not key it is still carried
    ListingKey.ofSlot(KinoApollo, slot) shouldBe ListingKey.of(KinoApollo, pageless)
  }

  it should "carry a page-keyed listing's year and director, which its page's enrichment wrote" in {
    val paged = CinemaMovie(Movie("Lalka"), KinoApollo, None, Some("https://kinoapollo.pl/film/lalka"), None, Nil, Nil, Nil)
    val slot  = slots.build(paged, "Lalka", Some(prior))
    (slot.releaseYear, slot.director) shouldBe ((Some(2026), Seq("Maciej Kawalski")))
    ListingKey.ofSlot(KinoApollo, slot) shouldBe ListingKey.of(KinoApollo, paged)
  }

  // Kino Iluzjon lists two "Lalka"s under two pages (Has 1968 at /2457, Kawalski 2026 at /7917, recorded PL corpus
  // 2026-10-05). Built over the slot the other page's enrichment wrote, the 2026 film's slot took Has and 1968 — a
  // venue slot whose year and director both deny its film.
  it should "carry nothing from a slot another page of the venue's wrote" in {
    val has     = prior.copy(releaseYear = Some(1968), director = Seq("Wojciech Jerzy Has"), synopsis = Some("Wokulski"),
      filmUrl = Some("https://www.iluzjon.fn.org.pl/filmy/info/2457/lalka.html"))
    val kawalski = CinemaMovie(Movie("Lalka"), KinoApollo, None, Some("https://www.iluzjon.fn.org.pl/filmy/info/7917/lalka.html"),
      None, Nil, Nil, Nil)
    val slot = slots.build(kawalski, "Lalka", Some(has))
    (slot.releaseYear, slot.director, slot.synopsis) shouldBe ((None, Nil, None))
    slots.build(kawalski, "Lalka", Some(has.copy(filmUrl = kawalski.filmUrl))).releaseYear shouldBe Some(1968)
  }

  // What Kino Iluzjon's 1968 "Lalka" slot held on prod (2026-10-06), written before the guard above: page /2457's url and
  // poster, every other detail page /7917's (2026, Kawalski, Dorociński, 162 min). Its url is its own page's, so the
  // guard carries all of it — until the page's own read says otherwise.
  it should "take a paged listing's carried detail from its page's read wherever the slot it is built over disagrees" in {
    val page     = "https://www.iluzjon.fn.org.pl/filmy/info/2457/lalka.html"
    val polluted = prior.copy(filmUrl = Some(page), runtimeMinutes = Some(162), cast = Seq("Marcin Dorociński"),
      posterUrl = Some("https://www.iluzjon.fn.org.pl/lalka.jpg"), genres = Seq("Dramat"))
    val read     = models.SourceData(releaseYear = Some(1968), director = Seq("Wojciech Jerzy Has"), runtimeMinutes = Some(159),
      cast = Seq("Mariusz Dmochowski"), synopsis = Some("Wokulski"))
    val facts: VenuePageFacts = (cinema, p) => Option.when(cinema == KinoApollo && p == page)(read)
    val built    = new CinemaSlotBuilder(Country.Poland.language, new StringPool, facts)
    val has      = CinemaMovie(Movie("Lalka"), KinoApollo, None, Some(page), None, Nil, Nil, Nil)
    val slot     = built.build(has, "Lalka", Some(polluted))
    (slot.releaseYear, slot.director, slot.runtimeMinutes, slot.cast, slot.synopsis) shouldBe
      ((Some(1968), Seq("Wojciech Jerzy Has"), Some(159), Seq("Mariusz Dmochowski"), Some("Wokulski")))
    (slot.posterUrl, slot.genres) shouldBe ((polluted.posterUrl, Seq("Dramat")))   // what the page does not state is carried
    built.build(has.copy(director = Seq("W. J. Has")), "Lalka", Some(polluted)).director shouldBe Seq("W. J. Has") // the listing's own wins
    slots.build(has, "Lalka", Some(polluted)).releaseYear shouldBe Some(2026)    // no read: carried, as before
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

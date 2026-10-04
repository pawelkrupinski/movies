package services.movies

import models.{Country, Showtime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

/** Each value here is one a recorded corpus (2026-10-01/04) carried to a served card. */
class SlotFieldsSpec extends AnyFlatSpec with Matchers {

  "SlotFields.countries" should "fold every spelling to the language's canonical one, once" in {
    SlotFields.countries(Seq("Belgia", "Niderlandy", "Netherlands", " "), Country.Poland.language) shouldBe Seq("Belgia", "Holandia")
    SlotFields.countries(Seq("USA", "Kanada", "Canada"), Country.Poland.language) shouldBe Seq("USA", "Kanada")
    SlotFields.countries(Seq("Niderlandy", "Netherlands"), Country.UnitedKingdom.language) shouldBe Seq("Netherlands")
  }

  "SlotFields.genres" should "read a list printed as one label as its genres" in {
    SlotFields.genres(Seq("Biograficzny/Muzyczny")) shouldBe Seq("Biograficzny", "Muzyczny")
    SlotFields.genres(Seq("Animowany/Musical", "musical")) shouldBe Seq("Animowany", "Musical")
    SlotFields.genres(Seq("Dramat, Komedia | Sci-Fi; ")) shouldBe Seq("Dramat", "Komedia", "Sci-Fi")
    SlotFields.genres(Seq("Sci-Fi", "Science Fiction")) shouldBe Seq("Sci-Fi", "Science Fiction")
  }

  "SlotFields.url" should "make a link a venue printed followable, or drop it" in {
    SlotFields.url("https://www.amctheatres.com/showtimes/all/2026-10-16/AMC Riverview 14 GDX/all/146609294?affiliateCode=WWM") shouldBe
      Some("https://www.amctheatres.com/showtimes/all/2026-10-16/AMC%20Riverview%2014%20GDX/all/146609294?affiliateCode=WWM")
    SlotFields.url("https://prod5.agileticketing.net/websales/pages/ticketsearchcriteria.aspx?evtinfo=656236\\~cd61ed16&") shouldBe
      Some("https://prod5.agileticketing.net/websales/pages/ticketsearchcriteria.aspx?evtinfo=656236%5C~cd61ed16&")
    SlotFields.url("https:\\\\omniwebticketing9.com\\marquee\\wytheville\\?schdate=2026-12-17&perfix=70777") shouldBe
      Some("https://omniwebticketing9.com/marquee/wytheville/?schdate=2026-12-17&perfix=70777")
    SlotFields.url("/screen-3", Some("https://woodstocktheatre.org/movies/dune")) shouldBe Some("https://woodstocktheatre.org/screen-3")
    SlotFields.url("/app/assets/movie/zieS.jpg", Some("https://www.kinoroma.zabrze.pl/repertuar")) shouldBe
      Some("https://www.kinoroma.zabrze.pl/app/assets/movie/zieS.jpg")
    SlotFields.url("  https://kino.pl/film  ") shouldBe Some("https://kino.pl/film")
    SlotFields.url("/screen-3") shouldBe None
    SlotFields.url("") shouldBe None
    SlotFields.url(" ") shouldBe None
    SlotFields.url("https://roxyulverston.co.uk/book/<perdcode>?") shouldBe None
    SlotFields.url("https://-/checkout/showing/474882") shouldBe None
    SlotFields.url("https:///kup") shouldBe None
    SlotFields.url("javascript:buy()") shouldBe None
    SlotFields.url("https://www.Rose Theatre Starlight Room") shouldBe None
  }

  private def at(url: Option[String], room: Option[String] = None, format: List[String] = Nil) =
    Showtime(LocalDateTime.of(2026, 10, 6, 10, 0), url, room, format)

  "SlotFields.showtimes" should "keep a screening the listing printed twice once, and parallel screens apart" in {
    val booked = at(Some("https://bilety24.pl/kino/1231-lalka-165320?id=997261"))
    SlotFields.showtimes(Seq(booked, at(Some("https://www.bilety24.pl#"))), None) shouldBe Seq(booked)
    SlotFields.showtimes(Seq(at(Some("https://www.syndicatedbk.com")), booked), None) shouldBe Seq(booked)
    SlotFields.showtimes(Seq(booked, booked, at(None)), None) shouldBe Seq(booked)
    SlotFields.showtimes(Seq(at(None), at(None)), None) shouldBe Seq(at(None))
    val screen1 = at(Some("https://ticketing.com/sales/SCOBAR/book?perfcode=140025"))
    val screen2 = at(Some("https://ticketing.com/sales/SCOBAR/book?perfcode=140026"))
    SlotFields.showtimes(Seq(screen1, screen2), None) shouldBe Seq(screen1, screen2)
    SlotFields.showtimes(Seq(at(None, format = List("2D")), at(None, format = List("IMAX"))), None) should have size 2
    SlotFields.showtimes(Seq(at(None, room = Some("1")), at(None, room = Some("2"))), None) should have size 2
    SlotFields.showtimes(Seq(at(Some("/kup/1"))), Some("https://kino.pl/film/lalka")) shouldBe Seq(at(Some("https://kino.pl/kup/1")))
    SlotFields.showtimes(Seq(at(Some(""))), None) shouldBe Seq(at(None))
  }
}

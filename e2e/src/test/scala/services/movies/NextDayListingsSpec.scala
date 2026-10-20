package services.movies

import models.{CinemaCityChain, CinemaMovie, CinemaShowing, KinoMuza, Movie, Showtime, Tmdb}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDateTime

class NextDayListingsSpec extends AnyFlatSpec with Matchers {
  private val cutoff = LocalDateTime.of(2026, 10, 21, 0, 0)
  private def listing(title: String, at: LocalDateTime*) =
    CinemaMovie(Movie(title), KinoMuza, None, None, None, Nil, Nil, at.map(Showtime(_, None)))

  "the next day's listings" should "keep a listing the venue billed with no showtimes, as the venue still bills it" in {
    // Run 528: Kino Parczew billed 'Kot prot' with no showtimes; the next day dropped it, so the film lost
    // that venue's slot although its listing had not changed.
    val billed = listing("Kot prot")
    NextDayListings.of(Map(KinoMuza -> Seq(billed)), cutoff) shouldBe Map(KinoMuza -> Seq(billed))
  }

  it should "drop the showtimes before the cutoff, and a listing that had showtimes and has none left" in {
    val ending  = listing("Lawa", cutoff.minusHours(3))
    val running = listing("Lalka", cutoff.minusHours(3), cutoff.plusHours(18))
    NextDayListings.of(Map(KinoMuza -> Seq(ending, running)), cutoff) shouldBe
      Map(KinoMuza -> Seq(running.copy(showtimes = Seq(Showtime(cutoff.plusHours(18), None)))))
  }

  "a chain's network detail slot" should "be told apart from a venue's and an enrichment source's slot, however it is keyed" in {
    // Run 528 after 0f9282da4: the Cinema City detail slot is keyed by the bare network cinema, which a
    // CinemaShowing-only match missed, so its day-two arrival still read as churn.
    NextDayListings.isNetworkSlot(CinemaCityChain) shouldBe true
    NextDayListings.isNetworkSlot(CinemaShowing(CinemaCityChain, "straykids")) shouldBe true
    NextDayListings.isNetworkSlot(KinoMuza) shouldBe false
    NextDayListings.isNetworkSlot(CinemaShowing(KinoMuza, "kotprot")) shouldBe false
    NextDayListings.isNetworkSlot(Tmdb) shouldBe false
  }
}

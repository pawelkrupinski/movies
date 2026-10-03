package scripts

import models.SourceData
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scripts.VenuePageBackfill.{Slot, Stamp, plan}
import services.cinemas.common.FilmDetail
import services.venuepages.{VenuePage, VenuePageKey}

import java.time.Instant

class VenuePageBackfillSpec extends AnyFlatSpec with Matchers {

  private val at      = Instant.parse("2026-10-01T12:00:00Z")
  private val muranow = VenuePageKey("muranow", "https://kinomuranow.pl/film/dune")
  private val ccDune  = VenuePageKey("cinema-city", "https://www.cinema-city.pl/filmy/dune/1")
  private val ccLalka = VenuePageKey("cinema-city", "https://www.cinema-city.pl/filmy/lalka/2")
  private val read    = SourceData(director = Seq("Denis Villeneuve"), runtimeMinutes = Some(155), synopsis = Some("Sand."),
    filmUrl = Some(muranow.page))
  private def venue(film: String, cinema: String, page: String, data: SourceData = SourceData()) =
    Slot(film, s"$cinema␟${film.takeWhile(_ != '|')}", data.copy(filmUrl = Some(page)))

  "the seed" should "take a venue's own page from the slot that names it" in {
    val p = plan(Seq(Stamp(muranow, gone = false)), Seq(Slot("dune|2021", "Kino Muranów␟dune", read)), Set.empty, at)
    p.writes shouldBe Seq(VenuePage(muranow, VenuePage.Read(VenuePageBackfill.detailOf(read)), at))
    p.writes.head.outcome shouldBe VenuePage.Read(FilmDetail(synopsis = Some("Sand."), director = Seq("Denis Villeneuve"), runtimeMinutes = Some(155)))
  }

  it should "take a chain's page from the film's chain slot when the film names that chain only this page" in {
    val chain = Slot("dune|2021", "Cinema City", SourceData(director = Seq("Denis Villeneuve"), runtimeMinutes = Some(166)))
    val p = plan(Seq(Stamp(ccDune, gone = false)),
      Seq(venue("dune|2021", "Cinema City Arkadia", ccDune.page, SourceData(runtimeMinutes = Some(166))), chain), Set.empty, at)
    p.writes.map(_.outcome) shouldBe Seq(VenuePage.Read(VenuePageBackfill.detailOf(chain.data)))
  }

  it should "leave a chain page out when the film names that chain two pages, which the one shared slot cannot tell apart" in {
    // Cinema City's "Lalka" and "Ladies Night - Lalka" on one row: the shared slot holds whichever was written last.
    val p = plan(Seq(Stamp(ccDune, gone = false), Stamp(ccLalka, gone = false)),
      Seq(venue("lalka|2026", "Cinema City Arkadia", ccDune.page), venue("lalka|2026", "Cinema City Bonarka", ccLalka.page),
        Slot("lalka|2026", "Cinema City", SourceData(director = Seq("Maciej Kawalski")))), Set.empty, at)
    p.writes shouldBe empty
    p.chainAmbiguous.toSet shouldBe Set(ccDune, ccLalka)
  }

  it should "seed a gone page as gone, and leave alone a page venue_pages already holds" in {
    val p = plan(Seq(Stamp(ccLalka, gone = true), Stamp(muranow, gone = false)),
      Seq(Slot("dune|2021", "Kino Muranów␟dune", read)), Set(muranow.id), at)
    p.writes shouldBe Seq(VenuePage(ccLalka, VenuePage.Gone(404), at))
    p.alreadyStored shouldBe 1
  }

  it should "seed a page stamped both read and gone as read: the page came back" in {
    // `freshness` scans in `_id` order, so the `|gone` stamp arrives before the `|read` one.
    val p = plan(Seq(Stamp(muranow, gone = true), Stamp(muranow, gone = false)),
      Seq(Slot("dune|2021", "Kino Muranów␟dune", read)), Set.empty, at)
    p.writes shouldBe Seq(VenuePage(muranow, VenuePage.Read(VenuePageBackfill.detailOf(read)), at))
  }

  it should "count a read page no slot names any more, writing nothing for it" in {
    val p = plan(Seq(Stamp(muranow, gone = false)), Nil, Set.empty, at)
    p.writes shouldBe empty
    p.readWithoutSlot shouldBe Seq(muranow)
  }

  // Pages read before pages had stamps of their own carry only the FILM's marker. The group's page-stamped
  // pages say which venues it reads; the film's slot at one of those venues names the page.
  "a per-film read marker" should "name the film's page at the group's venue, learned from the group's stamped pages" in {
    val stamped = Seq(Stamp(muranow, gone = false))                                  // Muranów's group reads Kino Muranów
    val older   = Slot("lalka|2026", "Kino Muranów␟lalka", SourceData(director = Seq("Maciej Kawalski"), filmUrl = Some("https://kinomuranow.pl/film/lalka")))
    val elsewhere = Slot("lalka|2026", "Kino Iluzjon␟lalka", SourceData(filmUrl = Some("https://iluzjon.fn.org.pl/lalka")))
    val (marked, unplaced) = VenuePageBackfill.markerStamps(
      Seq(VenuePageBackfill.FilmMarker("muranow", "lalka|2026"), VenuePageBackfill.FilmMarker("never-stamped", "lalka|2026")),
      stamped, Seq(Slot("dune|2021", "Kino Muranów␟dune", read), older, elsewhere))
    marked shouldBe Seq(Stamp(VenuePageKey("muranow", "https://kinomuranow.pl/film/lalka"), gone = false))
    unplaced shouldBe 1                                                                // its venues are unknown: never guessed
  }
}

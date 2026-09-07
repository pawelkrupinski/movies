package tools

import models.{CityScreening, ResolvedMovie, ResolvedRatings}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The read-model snapshot guard compares two captures of one corpus up to the film
 *  ids, which follow the scrape's arrival order (see `FilmId`). */
class ReadModelSnapshotSpec extends AnyFlatSpec with Matchers {
  private def card(id: String, title: String, year: Int): ResolvedMovie =
    ResolvedMovie(_id = id, title = title, originalTitle = None, posterUrl = None, fallbackPosterUrls = Nil,
      runtimeMinutes = None, releaseYear = Some(year), genres = Nil, countries = Nil, directors = Nil, cast = Nil,
      synopsis = None, trailerUrls = Nil, weightedRating = 0.0,
      ratings = ResolvedRatings(imdb = None, imdbUrl = None, metascore = None, metacriticUrl = "", rottenTomatoes = None,
        rottenTomatoesUrl = "", filmweb = None, filmwebUrl = ""))

  private def screening(filmId: String, city: String): CityScreening =
    CityScreening(_id = s"$filmId|$city|Kino", filmId = filmId, city = city, cinema = "Kino", filmUrl = None, showtimes = Nil)

  "orderIndependent" should "make two captures that differ only in the ids they minted compare equal" in {
    val first  = ReadModelSnapshot.Snapshot(
      Seq(card("f0001", "Takie jest życie", 2025), card("f0001~cinemaitaliaoggi", "Cinema Italia Oggi: Takie jest życie", 2025)),
      Seq(screening("f0001", "poznan"), screening("f0001~cinemaitaliaoggi", "warszawa")))
    val second = ReadModelSnapshot.Snapshot(
      Seq(card("f0999~cinemaitaliaoggi", "Cinema Italia Oggi: Takie jest życie", 2025), card("f0999", "Takie jest życie", 2025)),
      Seq(screening("f0999~cinemaitaliaoggi", "warszawa"), screening("f0999", "poznan")))

    ReadModelSnapshot.render(ReadModelSnapshot.orderIndependent(first)) shouldBe
      ReadModelSnapshot.render(ReadModelSnapshot.orderIndependent(second))
    val stable = ReadModelSnapshot.orderIndependent(first)
    stable.movies.map(_._id) should contain theSameElementsAs Seq("takiejestżycie|2025", "takiejestżycie|2025~cinemaitaliaoggi")
    stable.screenings.map(_._id) should contain theSameElementsAs
      Seq("takiejestżycie|2025|poznan|Kino", "takiejestżycie|2025~cinemaitaliaoggi|warszawa|Kino")
  }

  it should "keep the real ids of two films that would share one stand-in, so a genuine collision still shows" in {
    val twins = ReadModelSnapshot.Snapshot(
      Seq(card("f0001", "Diuna", 2021), card("f0002", "Diuna", 2021)), Nil)

    ReadModelSnapshot.orderIndependent(twins).movies.map(_._id) shouldBe Seq("f0001", "f0002")
  }

  it should "still tell a changed card apart" in {
    val before = ReadModelSnapshot.Snapshot(Seq(card("f0001", "Diuna", 2021)), Nil)
    val after  = ReadModelSnapshot.Snapshot(Seq(card("f0002", "Diuna", 2021).copy(runtimeMinutes = Some(155))), Nil)

    ReadModelSnapshot.render(ReadModelSnapshot.orderIndependent(before)) should not be
      ReadModelSnapshot.render(ReadModelSnapshot.orderIndependent(after))
  }
}

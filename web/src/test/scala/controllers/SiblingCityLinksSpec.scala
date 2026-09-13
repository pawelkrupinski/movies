package controllers

import models.{CinemaCityWroclavia, Helios, MovieRecord, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.FakeRequest
import play.api.test.Helpers._

import java.time.LocalDateTime

/**
 * A film's per-city page (`/{city}/movie/{slug}`) is a near-duplicate of the
 * same film's page in every other city currently showing it — Google Search
 * Console showed this exact URL pattern going largely unindexed on kinowo.net,
 * because nothing in the page linked one city's copy to another's; the only
 * path between them was the sitemap. `MovieControllerService.citiesShowing`
 * and the "W innych miastach" popup it feeds are the fix: real, server-rendered
 * `<a href>`s from one city's film page to every sibling city's, present in the
 * markup whether or not a visitor ever opens the popup.
 */
class SiblingCityLinksSpec extends AnyFlatSpec with Matchers {

  private def recordShowingIn(title: String, cinemas: Source*): MovieRecord = {
    val now = LocalDateTime.now()
    MovieRecord(
      imdbId = Some("tt00000001"),
      data = cinemas.map(_ -> SourceData(
        title       = Some(title),
        releaseYear = Some(2025),
        showtimes   = Seq(models.Showtime(now.plusHours(2), None, None, Nil))
      )).toMap
    )
  }

  "a film's page" should "link to its own page in every other city currently showing it" in {
    val title = "Zmierzch"
    val record = recordShowingIn(title, Helios, CinemaCityWroclavia)
    val (ctrl, _) = TestMovieController.build(Seq((title, Some(2025), record)))

    val result = ctrl.filmBySlug("poznan", "zmierzch").apply(FakeRequest(GET, "/poznan/movie/zmierzch"))
    status(result) shouldBe OK
    val html = contentAsString(result)

    // Real <a href>, present in the server-rendered HTML regardless of the
    // popup's CSS visibility — this is the part a crawler has to be able to
    // see without executing the click that reveals it to a person.
    html should include("""<a href="/wroclaw/movie/zmierzch" class="other-cities-link">Wrocław</a>""")
    // Not linked to itself.
    html should not include """<a href="/poznan/movie/zmierzch" class="other-cities-link">"""
  }

  it should "render no trigger or popup when no other city is currently showing it" in {
    val title = "Odyseja"
    val record = recordShowingIn(title, Helios)
    val (ctrl, _) = TestMovieController.build(Seq((title, Some(2025), record)))

    val html = contentAsString(ctrl.filmBySlug("poznan", "odyseja").apply(FakeRequest(GET, "/poznan/movie/odyseja")))

    // The class NAMES are always present — `_filmDetailStyles` emits their CSS
    // rules on every film page regardless. It's the actual ELEMENTS (the
    // button, the modal's id) that must be absent for a film with nowhere
    // else to link to.
    html should not include """class="other-cities-trigger""""
    html should not include """id="other-cities-modal-backdrop""""
  }

  "MovieControllerService.citiesShowing" should "exclude a city whose only showings of the film have already ended" in {
    val title = "Stary Film"
    val now = LocalDateTime.now()
    val record = MovieRecord(
      imdbId = Some("tt00000002"),
      data = Map(
        Helios              -> SourceData(title = Some(title), releaseYear = Some(2020), showtimes = Seq(models.Showtime(now.plusHours(2), None, None, Nil))),
        CinemaCityWroclavia -> SourceData(title = Some(title), releaseYear = Some(2020), showtimes = Seq(models.Showtime(now.minusDays(1), None, None, Nil)))
      )
    )
    val (ctrl, readModel) = TestMovieController.build(Seq((title, Some(2020), record)))
    val service = new MovieControllerService(readModel)

    val filmId = readModel.filmSlugs.idFor("stary-film").getOrElse(fail("expected the fixture film to have a slug"))
    val siblings = service.citiesShowing(filmId, excluding = models.Poznan, country = models.Country.default, now)

    siblings shouldBe empty
  }
}

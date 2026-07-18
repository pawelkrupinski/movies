package clients.kino_dkf_rumcajs

import org.scalatest.OptionValues
import clients.tools.FakeHttpFetch
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import models.KinoDKFRumcajs
import services.cinemas.pl.KinoDKFRumcajsClient

import java.time.LocalDateTime

/** Replays the recorded `rumcajs.czest.pl` WordPress blog through the client:
 *  the `repertuar` category index → newest monthly post → the per-film
 *  "D.MM Title (Sala …)" headers in its `entry-content`.
 *
 *  DKF Rumcajs is a Dyskusyjny Klub Filmowy that screens roughly weekly
 *  (Mondays at 18:00) — its programme is sparse, so the year comes off the
 *  post title and the fixed 18:00 time off the venue's stated cadence (no time
 *  is printed per line). */
class KinoDKFRumcajsClientSpec extends AnyFlatSpec with Matchers with OptionValues {

  private val movies = new KinoDKFRumcajsClient(new FakeHttpFetch("kino-dkf-rumcajs"), KinoDKFRumcajs).fetch()

  "KinoDKFRumcajsClient" should "return a non-empty, single-cinema film list" in {
    movies should not be empty
    movies.map(_.cinema).toSet shouldBe Set(KinoDKFRumcajs)
    all(movies.map(_.showtimes)) should not be empty
  }

  it should "pin the Monday 2026-06-22 18:00 screening of \"Czarne i białe w kolorze\"" in {
    // Header line in the fixture: "22.06 Czarne i białe w kolorze – klasyka kina
    // … (Sala widowiskowa)" — room tag stripped, year from the post title,
    // 18:00 from the venue's fixed Monday slot.
    val film = movies.find(_.movie.title.startsWith("Czarne i białe w kolorze")).value
    film.showtimes.map(_.dateTime) should contain(LocalDateTime.of(2026, 6, 22, 18, 0))
    film.showtimes.flatMap(_.bookingUrl) shouldBe empty
  }

  it should "capture the director off the \"Reżyseria:\" line" in {
    val film = movies.find(_.movie.title.startsWith("Czarne i białe w kolorze")).value
    film.director should contain("Jean-Jacques Annaud")
  }
}

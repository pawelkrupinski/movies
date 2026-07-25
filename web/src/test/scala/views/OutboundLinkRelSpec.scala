package views

import testsupport.TestMessages.given

import controllers.{CinemaShowtimes, FilmSchedule}
import models.{Helios, Movie, MovieRecord, Poznan, Showtime}
import services.readmodel.TestReadModel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDate, LocalDateTime}

/**
 * Crawl hygiene on the OUTBOUND links: every third-party href we render —
 * rating-site profiles, cinema film pages, per-showtime booking deep-links —
 * carries `rel="nofollow"`.
 *
 * A single city listing emits ~3.7k of them (four rating badges per card plus
 * one per showtime), which is an order of magnitude more outbound than internal
 * links on the page. None of them is an editorial endorsement — they're
 * plumbing — so passing crawl equity down them is pure leakage, and the volume
 * alone reads as a link farm to a crawler sizing up a young domain.
 *
 * `noopener` rides along on the showings links, which had `target="_blank"`
 * without it.
 */
class OutboundLinkRelSpec extends AnyFlatSpec with Matchers {

  private implicit val city: models.City = Poznan

  private val baseTime = LocalDateTime.of(2026, 5, 13, 18, 0)

  private def schedule(cinemaFilmUrls: Seq[(models.Cinema, String)] = Nil): FilmSchedule =
    FilmSchedule(
      movie          = Movie("Test movie", Some(120)),
      posterUrl      = None,
      synopsis       = None,
      cast           = Seq.empty,
      director       = Seq.empty,
      cinemaFilmUrls = cinemaFilmUrls,
      showings       = Seq(LocalDate.of(2026, 5, 13) -> Seq(CinemaShowtimes(Helios, Seq(
        Showtime(baseTime, Some("https://helios.pl/book/1"), Some("Sala 1"), List("2D"))
      )))),
      resolved       = TestReadModel.resolved("Test movie", None, MovieRecord())
    )

  // Every `<a>` pointing off-site must be nofollowed; nothing internal may be.
  private def externalAnchors(html: String): Seq[String] =
    "<a\\b[^>]*>".r.findAllIn(html).toSeq.filter(_.contains("href=\"http"))

  "_ratingBadges" should "nofollow every rating-site link" in {
    val html = views.html._ratingBadges(Some(TestReadModel.ratings("Some Film", MovieRecord(
      imdbId         = Some("tt1"),
      imdbRating     = Some(7.0),
      metascore      = Some(78),
      rottenTomatoes = Some(86),
      filmwebRating  = Some(6.7),
    )))).body

    val anchors = externalAnchors(html)
    anchors should have size 4
    all (anchors) should include ("""rel="nofollow noopener"""")
  }

  "_filmShowings" should "nofollow the per-showtime booking deep-link" in {
    val html = views.html._filmShowings(schedule()).body
    html should include ("""href="https://helios.pl/book/1"""")
    externalAnchors(html).filter(_.contains("badge-time")) should not be empty
    all (externalAnchors(html)) should include ("""rel="nofollow noopener"""")
  }

  it should "nofollow the cinema's own page for the film" in {
    val html = views.html._filmShowings(schedule(Seq(Helios -> "https://helios.pl/film/test"))).body
    val label = externalAnchors(html).filter(_.contains("cinema-label-link"))
    label should have size 1
    all (label) should include ("""rel="nofollow noopener"""")
  }

  it should "leave the internal 'more showings' link followable" in {
    val html = views.html._filmShowings(schedule()).body
    html should include ("""class="showings-more"""")
    // The internal deep-link carries no rel at all — crawl equity stays in-site.
    "<a href=\"/poznan/film[^\"]*\"[^>]*>".r.findFirstIn(html).getOrElse("") should not include "nofollow"
  }

  "_filmDetailContent" should "nofollow the cinema links on the film page" in {
    val html   = views.html._filmDetailContent(schedule(Seq(Helios -> "https://helios.pl/film/test"))).body
    val cinema = externalAnchors(html).filter(_.contains("cinema-link"))
    cinema should have size 1
    all (cinema) should include ("""rel="nofollow noopener"""")
  }
}

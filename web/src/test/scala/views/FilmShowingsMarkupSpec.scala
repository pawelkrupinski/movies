package views

import testsupport.TestMessages.given

import controllers.{CinemaShowtimes, FilmSchedule, ShowingsMarkup}
import models.{Helios, Movie, MovieRecord, Poznan, Showtime}
import services.readmodel.TestReadModel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{LocalDate, LocalDateTime}

/**
 * The showtime pills' markup is paid once per slot, and a city listing renders every
 * slot it knows: 52,577 on New York on 2026-10-02, a 19.9 MB page whose every render
 * allocated ~320 MB of web-us's old generation. What these pin is that the pills carry
 * only what the page cannot get any other way — and that dropping the rest loses no
 * expiry: a slot whose lapse the day's base cannot give exactly keeps its own.
 */
class FilmShowingsMarkupSpec extends AnyFlatSpec with Matchers {

  private implicit val city: models.City = Poznan

  private def schedule(showings: (LocalDate, Seq[Showtime])*): FilmSchedule = scheduleWith(Nil, showings*)

  private def scheduleWith(cinemaFilmUrls: Seq[(models.Cinema, String)], showings: (LocalDate, Seq[Showtime])*): FilmSchedule =
    FilmSchedule(
      movie          = Movie("Test movie", Some(120)),
      posterUrl      = None,
      synopsis       = None,
      cast           = Seq.empty,
      director       = Seq.empty,
      cinemaFilmUrls = cinemaFilmUrls,
      showings       = showings.map { case (date, slots) => date -> Seq(CinemaShowtimes(Helios, slots)) },
      resolved       = TestReadModel.resolved("Test movie", None, MovieRecord()),
      slug           = controllers.FilmHref.slugOf("Test movie"),
      asOf           = showings.head._1
    )

  private def slot(at: LocalDateTime) = Showtime(at, Some(s"https://helios.pl/book/${at.getHour}"), None, Nil)
  private def expiresAt(at: LocalDateTime) = at.plus(Showtime.Grace).atZone(city.zoneId).toInstant.toEpochMilli
  private def pills(html: String) = """<a [^>]*class="badge-time"[^>]*>""".r.findAllIn(html).toSeq
  private def attr(tag: String, name: String) = s""" $name="([^"]*)"""".r.findFirstMatchIn(tag).map(_.group(1))
  private def render(showings: (LocalDate, Seq[Showtime])*) = views.html._filmShowings(schedule(showings*)).body

  private val day = LocalDate.of(2026, 5, 13)

  "_filmShowings" should "leave the clock time and the expiry off an ordinary pill" in {
    val html = render(day -> Seq(slot(day.atTime(18, 0)), slot(day.atTime(20, 45))))
    pills(html) should have size 2
    all (pills(html)) should (not include "data-time" and not include "data-expires")
    html should include (s"""data-expires-from="${ShowingsMarkup.expiresFrom(day, city.zoneId)}"""")
  }

  it should "give a pill's expiry exactly as its day's base plus its clock time" in {
    for (at <- Seq(day.atTime(0, 5), day.atTime(18, 0), day.atTime(23, 59)))
      ShowingsMarkup.expiresFrom(day, city.zoneId) + (at.getHour * 60L + at.getMinute) * 60000L shouldBe expiresAt(at)
  }

  // Poland falls back an hour at 03:00 on 2026-10-25: midnight is +02:00, the afternoon +01:00.
  it should "keep an explicit expiry where the day's base would be an hour out (DST)" in {
    val changeover = LocalDate.of(2026, 10, 25)
    val afternoon  = changeover.atTime(15, 0)
    pills(render(changeover -> Seq(slot(afternoon)))).head should include (s"""data-expires="${expiresAt(afternoon)}"""")
  }

  it should "keep an explicit expiry on a past-midnight slot filed under the previous day" in {
    val lateShow = day.plusDays(1).atTime(0, 30)
    pills(render(day -> Seq(slot(lateShow)))).head should include (s"""data-expires="${expiresAt(lateShow)}"""")
  }

  it should "emit no whitespace between a day's tags" in {
    val html = render(day -> Seq(slot(day.atTime(18, 0)), slot(day.atTime(20, 45))))
    val days = """<div class="date-group"[\s\S]*?(?=<a href="/)""".r.findAllIn(html).toSeq
    days should not be empty
    all (days) should not include regex (""">\s+<""")
  }

  // ── What the browser puts back (`_showingsHydrate`) ─────────────────────────

  it should "send a group's shared booking-URL prefix once, and each pill only the rest" in {
    val slots = Seq(slot(day.atTime(18, 0)), slot(day.atTime(20, 45)))
    val html  = render(day -> slots)
    val group = """<div class="cinema-group"[^>]*>""".r.findFirstIn(html).get
    val prefix = attr(group, "data-u").get
    prefix shouldBe "https://helios.pl/book/"
    all (pills(html)) should not include "href="
    pills(html).map(p => prefix + attr(p, "data-s").get) shouldBe slots.flatMap(_.bookingUrl)
  }

  it should "keep a lone pill's whole link, where a prefix would save nothing" in {
    val html = render(day -> Seq(slot(day.atTime(18, 0))))
    html should not include "data-u="
    pills(html).head should include ("""href="https://helios.pl/book/18"""")
  }

  it should "name each cinema once, in its visible label rather than a data-cinema attribute too" in {
    val html = render(day -> Seq(slot(day.atTime(18, 0))))
    html should not include "data-cinema="
    html should include (s"""<div class="cinema-label">${Helios.displayName}</div>""")
  }

  it should "carry the cinema's page for the film on its first day only" in {
    val html = views.html._filmShowings(scheduleWith(Seq(Helios -> "https://helios.pl/film/test"),
      day -> Seq(slot(day.atTime(18, 0))), day.plusDays(1) -> Seq(slot(day.plusDays(1).atTime(18, 0))))).body
    "https://helios.pl/film/test".r.findAllIn(html).size shouldBe 1
    """class="cinema-label-link"""".r.findAllIn(html).size shouldBe 2
  }
}

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
  // A pill is `.badge-time` with a whole link, or a bare `<a data-s>` the browser completes.
  private def pills(html: String) = """<a [^>]*(?:class="badge-time"|data-s=)[^>]*>""".r.findAllIn(html).toSeq
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

  it should "print each pill's time on the country's clock — 12-hour in the US, 24-hour elsewhere" in {
    val shows = day -> Seq(slot(day.atTime(0, 5)), slot(day.atTime(12, 30)), slot(day.atTime(19, 30)))
    def times(html: String) = """>(\d{1,2}:\d{2}(?: [AP]M)?)<""".r.findAllMatchIn(html).map(_.group(1)).toSeq
    val newYork = models.Country.UnitedStates.cities.find(_.zoneId == models.TimeZones.UsEastern).getOrElse(fail("no Eastern US city"))
    times(views.html._filmShowings(schedule(shows))(using newYork).body) shouldBe Seq("12:05 AM", "12:30 PM", "7:30 PM")
    times(render(shows)) shouldBe Seq("00:05", "12:30", "19:30")
  }

  it should "emit no whitespace between a day's tags" in {
    val html = render(day -> Seq(slot(day.atTime(18, 0)), slot(day.atTime(20, 45))))
    val days = """<div class="date-group"[\s\S]*?(?=<a href="/)""".r.findAllIn(html).toSeq
    days should not be empty
    all (days) should not include regex (""">\s+<""")
  }

  // A booking or cinema-page URL is scraped off someone else's site; only a web
  // address may become a link on ours, whatever scheme the upstream wrote.
  it should "never link a scraped URL that is not http(s)" in {
    val hostile = Seq("javascript:alert(document.cookie)", " JavaScript:alert(1)", "data:text/html,<script>alert(1)</script>")
    val slots   = hostile.zipWithIndex.map { case (url, i) => Showtime(day.atTime(18, i), Some(url), None, Nil) }
    val html    = views.html._filmShowings(scheduleWith(Seq(Helios -> "javascript:alert(2)"), day -> slots)).body
    html.toLowerCase should (not include "javascript:" and not include "data:text")
    // Still listed, just not as links.
    """<span class="badge-time"""".r.findAllIn(html).size shouldBe hostile.size
  }

  it should "keep linking http and https URLs" in {
    val slots = Seq(Showtime(day.atTime(18, 0), Some("http://kino.example/b/1"), None, Nil),
                    Showtime(day.atTime(20, 0), Some("HTTPS://kino.example/b/2"), None, Nil))
    val html  = views.html._filmShowings(scheduleWith(Seq(Helios -> "https://helios.pl/film"), day -> slots)).body
    pills(html) should have size 2
    html should include ("""href="https://helios.pl/film"""")
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
    // Not a link until the browser builds its href, so it carries nothing a link needs
    // either: `_showingsHydrate` adds the class, target and nofollow with the href.
    all (pills(html)) should (not include "class=" and not include "target=" and not include "rel=")
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
    """class="cinema-label-link"""".r.findAllIn(html).size shouldBe 1
    // The later day's label is a bare link the browser completes from the first one.
    html should include (s"""<div class="cinema-label"><a>${Helios.displayName} &#8599;</a></div>""")
  }

  // `ShowingsMarkup` escapes straight into its builder rather than through
  // `HtmlFormat.escape`; it must agree with Twirl on every character, or a cinema name
  // or a booking URL could break out of its attribute.
  // A Java string is one byte per char until its first char outside Latin-1, then two
  // for all of it. The cinema link's arrow went out as the literal `↗`, which doubled
  // every film's markup mid-render; as `&#8599;` it renders the same and keeps it narrow.
  it should "keep a film whose names are Latin-1 in Latin-1, cinema-page arrow included" in {
    // An English-language city: a Polish date label is outside Latin-1 on its own.
    val london = models.City.bySlug("london").getOrElse(fail("no city 'london'"))
    val html = views.html._filmShowings(scheduleWith(Seq(Helios -> "https://helios.pl/film/test"),
      day -> Seq(slot(day.atTime(18, 0))), day.plusDays(1) -> Seq(slot(day.plusDays(1).atTime(18, 0)))))(using london).body
    html should include ("&#8599;</a>")
    html.filter(_ > '\u00FF') shouldBe empty
  }

  "ShowingsMarkup.escapeInto" should "escape exactly as Twirl does, character for character" in {
    val everything = (0 to 0x2FF).map(_.toChar).mkString + "↗—Łódź\uD83C\uDFAC" + "<script>\"'&"
    val ours = new java.lang.StringBuilder
    ShowingsMarkup.escapeInto(ours, everything)
    ours.toString shouldBe play.twirl.api.HtmlFormat.escape(everything).body
  }
}

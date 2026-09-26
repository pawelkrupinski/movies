package clients.kinoprogramm

import clients.tools.{FakeHttpFetch, WriteKinoprogramm}
import models.{Cinema, GermanRoster}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.KinoprogrammClient
import tools.{GetOnlyHttpFetch, HttpFetch}

import java.time.{LocalDate, LocalDateTime}
import scala.collection.mutable.ListBuffer

/** Replays two venues' real kinoprogramm.com weeks, recorded on 2026-09-26 by
 *  [[WriteKinoprogramm]]: CineStar Kulturbrauerei (a multiplex) and 3001 Kino. */
class KinoprogrammClientSpec extends AnyFlatSpec with Matchers {

  private val Today = LocalDate.of(2026, 9, 26)
  private val paths = WriteKinoprogramm.Venues.toMap
  private def cinema(theaterId: String): Cinema =
    GermanRoster.theaterIdByCinema.collectFirst { case (c, id) if id == theaterId => c }.get

  private class Counting(inner: HttpFetch) extends GetOnlyHttpFetch {
    val urls = ListBuffer.empty[String]
    def get(url: String): String = { urls += url; inner.get(url) }
  }

  private def scrape(theaterId: String) = {
    val fetch  = new Counting(new FakeHttpFetch("kinoprogramm"))
    val movies = new KinoprogrammClient(fetch, paths(theaterId), cinema(theaterId), today = Some(Today)).fetch()
    (movies, fetch.urls.toList)
  }

  private lazy val (cineStar, cineStarUrls) = scrape("A0738")

  "KinoprogrammClient" should "read every film and showtime the venue's weeks list" in {
    cineStar should have size 36
    cineStar.flatMap(_.showtimes) should have size 142
    cineStar.flatMap(_.showtimes).map(_.dateTime.toLocalDate).max shouldBe LocalDate.of(2026, 10, 23)
  }

  // The page is a 7-day grid; the rest of the programme is only reachable a week at
  // a time, and a fixed window would hide it — the horizon rule every client keeps.
  it should "walk the programme a week at a time until three blank weeks, not stop at the first page" in {
    val weeks = cineStarUrls.map(_.split("datum=")(1)).map(LocalDate.parse)
    weeks.head shouldBe Today
    weeks.sliding(2).foreach { case Seq(a, b) => b shouldBe a.plusWeeks(1); case _ => () }
    weeks.last.isAfter(LocalDate.of(2026, 10, 23)) shouldBe true
    val (_, arthouseUrls) = scrape("A0002")
    arthouseUrls should have size 4   // one week with a programme, then three blank ones
  }

  it should "carry the film's title, runtime, genre and FSK rating, and its kinoprogramm page" in {
    val spiderMan = cineStar.find(_.movie.title == "Spider-Man: Brand New Day").get
    spiderMan.movie.runtimeMinutes shouldBe Some(139)
    spiderMan.movie.genres shouldBe Seq("Action")
    spiderMan.ageRating shouldBe Some("FSK 12")
    spiderMan.filmUrl shouldBe Some(
      "https://www.kinoprogramm.com/kino/berlin/cinestar-kino-in-der-kulturbrauerei/spiderman:-brand-new-day-406580")
    spiderMan.cinema shouldBe cinema("A0738")
  }

  // "Deutsch" is a German film or a German dub, which the page does not tell
  // apart: it stays unmarked, as a German film is on Filmstarts. Original with
  // subtitles is Filmstarts' "OmU".
  it should "tag original-with-subtitles screenings OmU and leave German ones unmarked" in {
    val spiderMan = cineStar.find(_.movie.title == "Spider-Man: Brand New Day").get
    def at(d: Int, h: Int, m: Int) = spiderMan.showtimes.find(_.dateTime == LocalDateTime.of(2026, 9, d, h, m)).get
    at(26, 20, 20).format shouldBe Nil
    at(26, 22, 20).format shouldBe List("OmU")
    at(28, 16, 45).format shouldBe List("OmU")
    spiderMan.showtimes.map(_.dateTime.toLocalDate).take(6) shouldBe
      Seq(26, 26, 27, 28, 28, 29).map(LocalDate.of(2026, 9, _))
  }

  it should "list each film once, its showtimes in time order" in {
    cineStar.map(_.movie.title).distinct should have size cineStar.size
    cineStar.foreach(m => m.showtimes.map(_.dateTime) shouldBe m.showtimes.map(_.dateTime).sorted)
  }

  // A page without the programme list is a changed layout or a block page, never a
  // venue with nothing on — so a walk that only ever sees such pages fails the
  // scrape rather than serving an empty fallback.
  it should "fail rather than read a page without its programme list as an empty venue" in {
    val blocked = new GetOnlyHttpFetch { def get(url: String): String = "<html><body><h1>Forbidden</h1></body></html>" }
    an[IllegalStateException] should be thrownBy
      new KinoprogrammClient(blocked, paths("A0738"), cinema("A0738"), today = Some(Today)).fetch()
  }
}

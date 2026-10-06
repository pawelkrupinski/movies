package clients.kinoprogramm

import clients.tools.{FakeHttpFetch, FixtureFile, WriteKinoprogramm}
import models.{Cinema, GermanRoster}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.common.KinoprogrammClient
import tools.{GetOnlyHttpFetch, HttpFetch}

import java.time.{LocalDate, LocalDateTime}
import scala.collection.mutable.ListBuffer

/** Replays two venues' real kinoprogramm.com weeks, recorded on 2026-09-26 by
 *  [[WriteKinoprogramm]]: CineStar Kulturbrauerei (a multiplex) and 3001 Kino — and
 *  their films' catalogue pages (`/kinofilm/…`), recorded on 2026-10-06. */
class KinoprogrammClientSpec extends AnyFlatSpec with Matchers {

  private val Today = LocalDate.of(2026, 9, 26)
  private val paths = WriteKinoprogramm.Venues.toMap
  private def cinema(theaterId: String): Cinema =
    GermanRoster.theaterIdByCinema.collectFirst { case (c, id) if id == theaterId => c }.get

  private class Counting(inner: HttpFetch) extends GetOnlyHttpFetch {
    val urls = ListBuffer.empty[String]
    def get(url: String): String = { urls += url; inner.get(url) }
  }

  // Film catalogue pages replay apart from the weeks, so the walk's URL counts stay the weeks'.
  private val filmPages = new FakeHttpFetch("kinoprogramm")

  private def scrape(theaterId: String) = {
    val fetch  = new Counting(new FakeHttpFetch("kinoprogramm"))
    val movies = new KinoprogrammClient(fetch, paths(theaterId), cinema(theaterId), today = Today, filmPages = filmPages).fetch()
    (movies, fetch.urls.toList)
  }

  private lazy val (cineStar, cineStarUrls) = scrape("A0738")

  "KinoprogrammClient" should "read every film and showtime the venue's weeks list" in {
    // 36 listed; CineStar's mystery screening "CineSneak" names no film and is dropped.
    cineStar should have size 35
    cineStar.map(_.movie.title) should contain ("Die Tribute von Panem - The Hunger Games")
    cineStar.flatMap(_.showtimes) should have size 141   // CineSneak's one slot gone
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

  // The identity resolver weighs a venue's synopsis, director, cast, year and poster as
  // evidence; the grid carries the poster and trailer, the film's catalogue page the rest.
  it should "carry the film's synopsis, cast, year and country from its catalogue page, poster and trailer from the grid" in {
    val spiderMan = cineStar.find(_.movie.title == "Spider-Man: Brand New Day").get
    spiderMan.synopsis.get should startWith ("Nach dem phänomenalen weltweiten Erfolg von \"Spider-Man: No Way Home\"")
    spiderMan.cast shouldBe Seq("Tom Holland", "Zendaya", "Jacob Batalon", "Sadie Sink", "Jon Bernthal", "Mark Ruffalo")
    spiderMan.movie.releaseYear shouldBe Some(2026)
    spiderMan.movie.countries shouldBe Seq("USA")
    spiderMan.posterUrl shouldBe Some("https://www.kinoprogramm.com/media/images/poster/p406580_312.jpg")
    spiderMan.trailerUrl shouldBe Some("https://www.kinoprogramm.com/media/trailer/420396_h720.mp4")   // the trailer has its own id
  }

  // A re-release: the catalogue's 2006 is what tells "Pan's Labyrinth" from any new film of
  // the name, and its synopsis names the cast it stars. The distributor credit closing the
  // text is attribution, not story.
  it should "carry the director, and the synopsis without its source credit" in {
    val (arthouse, _) = scrape("A0002")
    val pan = (cineStar ++ arthouse).find(_.movie.title.startsWith("Pan")).get
    pan.director shouldBe Seq("Guillermo del Toro")
    pan.movie.releaseYear shouldBe Some(2006)
    pan.synopsis.get should include ("(Ivana Baquero)")
    (cineStar ++ arthouse).flatMap(_.synopsis).filter(_.contains("Quelle:")) shouldBe empty
    cineStar.count(_.synopsis.isDefined) shouldBe cineStar.size
  }

  // The catalogue page is the same for every venue, and the composition root's cache dedupes
  // it across them; within one scrape a film listed every week is still asked once.
  it should "read each film's catalogue page once, on the film's own path" in {
    val asked = new Counting(filmPages)
    new KinoprogrammClient(new FakeHttpFetch("kinoprogramm"), paths("A0738"), cinema("A0738"), today = Today, filmPages = asked).fetch()
    asked.urls.distinct should have size asked.urls.size.toLong
    asked.urls should have size cineStar.size.toLong
    asked.urls should contain ("https://www.kinoprogramm.com/kinofilm/spiderman:-brand-new-day-406580")
  }

  // The fallback exists for the showtimes; a catalogue page that cannot be read costs the
  // film its synopsis and credits, never its place in the programme.
  it should "serve a film whose catalogue page fails with the grid's fields alone" in {
    val failing = new GetOnlyHttpFetch { def get(url: String): String = throw new java.io.IOException("connection reset") }
    val movies  = new KinoprogrammClient(new FakeHttpFetch("kinoprogramm"), paths("A0738"), cinema("A0738"), today = Today, filmPages = failing).fetch()
    movies.map(_.movie.title) shouldBe cineStar.map(_.movie.title)
    movies.flatMap(_.synopsis) shouldBe empty
    movies.flatMap(_.posterUrl) should not be empty
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

  // Two films can share a title (a remake, a re-release) and each has its own page;
  // merged by title they would read as one film with the other's showtimes.
  it should "keep two films that share a title apart, by their own pages" in {
    val real = new FakeHttpFetch("kinoprogramm")
    val twinned = new GetOnlyHttpFetch {
      def get(url: String): String = {
        val page = real.get(url)
        if (!url.endsWith(s"datum=$Today")) page
        else {
          val start   = page.indexOf("<article ")
          val article = page.substring(start, page.indexOf("</article>", start) + "</article>".length)
          val filmId  = """id="film-(\d+)"""".r.findFirstMatchIn(article).get.group(1)
          val twin    = article.replace(s"-$filmId", "-999999")
          page.substring(0, start) + twin + page.substring(start)
        }
      }
    }
    val movies = new KinoprogrammClient(twinned, paths("A0738"), cinema("A0738"), today = Today, filmPages = filmPages).fetch()
    val title  = movies.groupBy(_.movie.title).collectFirst { case (t, same) if same.size == 2 => t }
    title shouldBe defined
    movies.filter(m => title.contains(m.movie.title)).flatMap(_.filmUrl).exists(_.endsWith("-999999")) shouldBe true
  }

  // kinoprogramm.com lists a venue's mystery screening as a film of its own ("Sneak Preview" at
  // Abaton, 2026-10-04); Filmstarts gives the same slot no film at all. It names no film and
  // could only render as a card nothing resolves — the non-film event filter drops it.
  it should "drop a Sneak Preview, which names no film" in {
    val real = new FakeHttpFetch("kinoprogramm")
    val sneaky = new GetOnlyHttpFetch {
      def get(url: String): String = {
        val page = real.get(url)
        if (!url.endsWith(s"datum=$Today")) page
        else {
          val doc = org.jsoup.Jsoup.parse(page)
          import scala.jdk.CollectionConverters._
          doc.selectFirst("article[data-kino-week-film]").select("a[href]").asScala.find(_.text.trim.nonEmpty).get.text("Sneak Preview")
          doc.outerHtml
        }
      }
    }
    val movies = new KinoprogrammClient(sneaky, paths("A0738"), cinema("A0738"), today = Today, filmPages = filmPages).fetch()
    movies.map(_.movie.title) should not contain "Sneak Preview"
    movies should have size (cineStar.size - 1)
  }

  // A page without the programme list is a changed layout or a block page, never a
  // venue with nothing on — so a walk that only ever sees such pages fails the
  // scrape rather than serving an empty fallback.
  it should "fail rather than read a page without its programme list as an empty venue" in {
    val blocked = new GetOnlyHttpFetch { def get(url: String): String = "<html><body><h1>Forbidden</h1></body></html>" }
    an[IllegalStateException] should be thrownBy
      new KinoprogrammClient(blocked, paths("A0738"), cinema("A0738"), today = Today, filmPages = filmPages).fetch()
  }

  // kinoprogramm's slugs keep their accents; the request line must be ASCII.
  "KinoprogrammClient.catalogueUrl" should "percent-encode an accented slug, and want the film id" in {
    KinoprogrammClient.catalogueUrl("/kino/lingen/filmpalast-cineworld/andré-rieus-weihnachtskonzert-2026:-let-it-snow-419955") shouldBe
      Some("https://www.kinoprogramm.com/kinofilm/andr%C3%A9-rieus-weihnachtskonzert-2026:-let-it-snow-419955")
    KinoprogrammClient.catalogueUrl("/kino/berlin/some-venue/no-film-id") shouldBe None
  }

  // Recorded 2026-10-06 from
  // https://www.kinoprogramm.com/kinofilm/andré-rieus-weihnachtskonzert-2026:-let-it-snow-419955.
  // Its synopsis is the catalogue's, copied from the 2025 concert ("Merry Christmas") under the
  // 2026 title — the facts are kinoprogramm's catalogue entry, not the venue's.
  "KinoprogrammClient.parseFilmPage" should "read a director-only catalogue page" in {
    val detail = KinoprogrammClient.parseFilmPage(FixtureFile.read("test/resources/fixtures/kinoprogramm/film_page_andre_rieu_christmas_2026.html"))
    detail.director shouldBe Seq("André Rieu")
    detail.cast shouldBe empty
    detail.releaseYear shouldBe Some(2026)
    detail.countries shouldBe Seq("Niederlande")
    detail.synopsis.get should (include ("Emma Kok") and endWith ("exklusiv im Kino!"))
  }
}

package services.cinemas

import org.scalatest.matchers.should.Matchers
import models._
import org.scalatest.flatspec.AnyFlatSpec
import services.cinemas.common.{ScrapedSlot, SlotsToMovies}

import java.time.LocalDateTime

/** Guards the invariant fold every hand-rolled cinema scraper shares via
  * [[SlotsToMovies.fold]]: group by a film key, dedup + time-sort the showtimes,
  * drop a film with no showtimes, and order the result. A regression here
  * would silently change the merged output of ~100 scrapers at once. */
class SlotsToMoviesSpec extends AnyFlatSpec with Matchers {

  private val cinema = KinoMuza
  private def dt(h: Int) = LocalDateTime.of(2026, 6, 8, h, 0)
  private case class Slot(title: String, dateTime: LocalDateTime, booking: Option[String])

  private def fold(slots: Seq[Slot]): Seq[CinemaMovie] =
    SlotsToMovies.fold(
      slots,
      keyOf      = _.title,
      showtimeOf = s => Showtime(s.dateTime, s.booking)
    ) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = group.map(_.booking).headOption.flatten,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }

  "fold" should "group slots by title into one CinemaMovie per title" in {
    val out = fold(Seq(
      Slot("Dune", dt(18), Some("b1")),
      Slot("Dune", dt(20), Some("b2")),
      Slot("Wicked", dt(19), Some("b3"))
    ))
    out.map(_.movie.title) shouldBe Seq("Dune", "Wicked")
    out.head.showtimes.map(_.dateTime) shouldBe Seq(dt(18), dt(20))
  }

  it should "dedup showtimes by (dateTime, bookingUrl) and sort by time" in {
    val out = fold(Seq(
      Slot("Dune", dt(20), Some("b1")),
      Slot("Dune", dt(18), Some("b1")),
      Slot("Dune", dt(20), Some("b1")) // exact duplicate
    ))
    out.head.showtimes.map(_.dateTime) shouldBe Seq(dt(18), dt(20))
  }

  it should "keep same-time screenings with different booking links" in {
    val out = fold(Seq(
      Slot("Dune", dt(20), Some("b1")),
      Slot("Dune", dt(20), Some("b2"))
    ))
    out.head.showtimes should have size 2
  }

  it should "drop a film whose slots all yield no showtimes" in {
    // With a distinctBy that collapses everything and an empty source there are
    // no showtimes, so the film is dropped.
    fold(Seq.empty) shouldBe empty
  }

  it should "sort the resulting movies by title" in {
    val out = fold(Seq(
      Slot("Zorro", dt(18), None),
      Slot("Amelia", dt(18), None)
    ))
    out.map(_.movie.title) shouldBe Seq("Amelia", "Zorro")
  }

  it should "honour a custom distinctBy key" in {
    val out = SlotsToMovies.fold(
      Seq(Slot("Dune", dt(20), Some("b1")), Slot("Dune", dt(20), Some("b2"))),
      keyOf      = _.title,
      showtimeOf = s => Showtime(s.dateTime, s.booking),
      distinctBy = _.dateTime // dedup on time alone, ignoring booking
    ) { (title, _, showtimes) =>
      CinemaMovie(Movie(title), cinema, None, None, None, Nil, Nil, showtimes)
    }
    out.head.showtimes should have size 1
  }

  private def plain(key: String, showtimes: Seq[Showtime]) =
    CinemaMovie(Movie(key), cinema, None, None, None, Nil, Nil, showtimes)

  it should "group by a key other than the title and hand the builder that key" in {
    val out = SlotsToMovies.fold(
      Seq(Slot("Dune", dt(18), Some("b1")), Slot("Dune: Part Two", dt(19), Some("b1"))),
      keyOf      = _.booking,
      showtimeOf = s => Showtime(s.dateTime, s.booking)
    ) { (key, group, showtimes) => plain(s"${key.getOrElse("")}:${group.head.title}", showtimes) }
    out.map(_.movie.title) shouldBe Seq("b1:Dune")
    out.head.showtimes should have size 2
  }

  it should "keep every showtime, a repeated one too, under keepEvery" in {
    val out = SlotsToMovies.fold(
      Seq(Slot("Dune", dt(20), Some("b1")), Slot("Dune", dt(20), Some("b1"))),
      keyOf      = _.title,
      showtimeOf = s => Showtime(s.dateTime, s.booking),
      distinctBy = SlotsToMovies.keepEvery
    )((title, _, showtimes) => plain(title, showtimes))
    out.head.showtimes should have size 2
  }

  it should "keep groupBy's order under asGrouped and the key's under byKey" in {
    val slots  = ('a' to 'h').map(c => Slot(s"$c", dt(18), None)).reverse
    val keyOf  = (s: Slot) => 'z' - s.title.head
    def titles(order: SlotsToMovies.MovieOrder[Int]) =
      SlotsToMovies.fold(slots, keyOf, s => Showtime(s.dateTime, s.booking), order = order)((_, group, showtimes) =>
        plain(group.head.title, showtimes)).map(_.movie.title)
    titles(SlotsToMovies.MovieOrder.asGrouped) shouldBe slots.groupBy(keyOf).toSeq.map(_._2.head.title)
    titles(SlotsToMovies.MovieOrder.byKey) shouldBe Seq("h", "g", "f", "e", "d", "c", "b", "a")
    titles(SlotsToMovies.MovieOrder.byTitle) shouldBe Seq("a", "b", "c", "d", "e", "f", "g", "h")
  }

  "foldSome" should "drop a film its builder refuses" in {
    val out = SlotsToMovies.foldSome(
      Seq(Slot("Dune", dt(18), None), Slot("Wicked", dt(19), None)),
      keyOf      = _.title,
      showtimeOf = s => Showtime(s.dateTime, s.booking)
    )((title, _, showtimes) => Option.when(title != "Wicked")(plain(title, showtimes)))
    out.map(_.movie.title) shouldBe Seq("Dune")
  }

  "foldBlocks" should "merge every block's showtimes, deduped and time-sorted, and drop a film with none" in {
    case class Block(title: String, times: Seq[Int])
    val out = SlotsToMovies.foldBlocks(
      Seq(Block("Dune", Seq(20, 18)), Block("Dune", Seq(18, 16)), Block("Wicked", Nil)),
      keyOf       = _.title,
      showtimesOf = b => b.times.map(h => Showtime(dt(h), None))
    )((title, _, showtimes) => Some(plain(title, showtimes)))
    out.map(_.movie.title) shouldBe Seq("Dune")
    out.head.showtimes.map(_.dateTime) shouldBe Seq(dt(16), dt(18), dt(20))
  }

  "mergeMovies" should "merge one film's rows under the first one's metadata, showtimes deduped and time-sorted" in {
    val first  = plain("Dune", Seq(Showtime(dt(20), None), Showtime(dt(18), None))).copy(posterUrl = Some("p1"))
    val second = plain("Dune", Seq(Showtime(dt(18), None), Showtime(dt(16), None))).copy(posterUrl = Some("p2"))
    val empty  = plain("Wicked", Nil)
    val out = SlotsToMovies.mergeMovies(Seq(first, second, empty), _.movie.title)
    out.map(_.movie.title) shouldBe Seq("Dune")
    out.head.posterUrl shouldBe Some("p1")
    out.head.showtimes.map(_.dateTime) shouldBe Seq(dt(16), dt(18), dt(20))
  }

  // The plain-listing clients (Nowa Fala, Kijów, Amok, ...) moved onto `basic` from their own folds: each took a film's
  // poster and page as the FIRST its slots carry (a slot without one does not hide a later slot's), and each slot's
  // hall and format onto its showtime. A different pick would silently swap posters across those venues.
  "basic" should "take a film's poster and page as the first its slots carry, and each slot's room and format" in {
    val out = SlotsToMovies.basic(Seq(
      ScrapedSlot("Dune", dt(18), bookingUrl = Some("b1")),
      ScrapedSlot("Dune", dt(20), bookingUrl = Some("b2"), filmUrl = Some("f2"), posterUrl = Some("p2"), room = Some("Sala 2"), format = List("3D")),
      ScrapedSlot("Dune", dt(21), filmUrl = Some("f3"), posterUrl = Some("p3")),
      ScrapedSlot("Wicked", dt(19))
    ), cinema)
    out.map(_.movie.title) shouldBe Seq("Dune", "Wicked")
    val dune = out.head
    dune.posterUrl shouldBe Some("p2")
    dune.filmUrl shouldBe Some("f2")
    dune.cinema shouldBe cinema
    dune.showtimes.map(s => (s.dateTime, s.bookingUrl, s.room, s.format)) shouldBe Seq(
      (dt(18), Some("b1"), None, Nil), (dt(20), Some("b2"), Some("Sala 2"), List("3D")), (dt(21), None, None, Nil))
    out(1).posterUrl shouldBe None
    out(1).filmUrl shouldBe None
  }

  it should "dedup by time and booking link unless the client names its own key" in {
    val slots = Seq(
      ScrapedSlot("Dune", dt(20), bookingUrl = Some("b1"), room = Some("Sala 1")),
      ScrapedSlot("Dune", dt(20), bookingUrl = Some("b2"), room = Some("Sala 1")),
      ScrapedSlot("Dune", dt(20), bookingUrl = Some("b1"), room = Some("Sala 2")))
    SlotsToMovies.basic(slots, cinema).head.showtimes.map(s => (s.bookingUrl, s.room)) shouldBe
      Seq((Some("b1"), Some("Sala 1")), (Some("b2"), Some("Sala 1")))
    SlotsToMovies.basic(slots, cinema, distinctBy = SlotsToMovies.byTimeAndRoom).head.showtimes.map(s => (s.bookingUrl, s.room)) shouldBe
      Seq((Some("b1"), Some("Sala 1")), (Some("b1"), Some("Sala 2")))
  }
}

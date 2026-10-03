package views

import testsupport.TestMessages.given

import controllers.{CaffeineFilmCardFragments, CinemaShowtimes, FilmCardFragments, FilmSchedule}
import models.{Cinema, City, Movie, MovieRecord, Showtime}
import services.readmodel.TestReadModel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.lang.management.ManagementFactory
import java.time.LocalDate

/**
 * A listing re-rendered with its films unchanged writes their cards from
 * [[CaffeineFilmCardFragments]] instead of rendering them again — byte for byte the
 * page an uncached render produces, and a fresh card the moment anything it shows moves.
 */
class FilmCardFragmentsSpec extends AnyFlatSpec with Matchers {

  private implicit val city: City = City.bySlug("poznan").getOrElse(fail("no city 'poznan'"))
  private val day = LocalDate.of(2026, 5, 13)
  private val cinemas = Cinema.all.distinct.filter(c => City.forCinema(c).contains(city)).take(6)

  private def film(n: Int, firstHour: Int = 10, imdb: Double = 7.1, cinemas: Seq[Cinema] = cinemas): FilmSchedule = FilmSchedule(
    movie = Movie(s"Film $n", Some(110)), posterUrl = Some(s"https://image.example/$n.jpg"), synopsis = None,
    cast = Seq("Actor One", "Actor Two"), director = Seq("A Director"),
    cinemaFilmUrls = cinemas.take(2).map(c => c -> s"https://kino.example/film-$n"),
    showings = (0 until 4).map { d =>
      val date = day.plusDays(d.toLong)
      date -> cinemas.map(c => CinemaShowtimes(c, (0 until 5).map(s =>
        Showtime(date.atTime(firstHour + s * 2, 15), Some(s"https://kino.example/book?film=$n&date=$date&id=${n * 100 + s}"), None, Nil))))
    },
    resolved = TestReadModel.resolved(s"Film $n", Some(2026), MovieRecord(imdbId = Some(s"tt$n"), imdbRating = Some(imdb))),
    slug = Some(s"film-$n"), asOf = day)

  private def listing(films: Seq[FilmSchedule], fragments: FilmCardFragments): String =
    views.html._filmCards(films)(using city, summon, fragments).body

  "a listing rendered through the card cache" should "be byte for byte the uncached page" in {
    val films = (0 until 3).map(film(_))
    val uncached = listing(films, FilmCardFragments.Uncached)
    val cache = new CaffeineFilmCardFragments(CaffeineFilmCardFragments.DefaultMaxBytes)
    listing(films, cache) shouldBe uncached
    listing(films, cache) shouldBe uncached          // ...and again, from the cache
    cache.occupancy.hitRatio.get shouldBe 0.5
  }

  it should "render a card again the moment its showings change, and only that card" in {
    val cache = new CaffeineFilmCardFragments(CaffeineFilmCardFragments.DefaultMaxBytes)
    val before = listing(Seq(film(1), film(2)), cache)
    val moved  = Seq(film(1), film(2, firstHour = 11))
    val after  = listing(moved, cache)
    after should not be before
    after shouldBe listing(moved, FilmCardFragments.Uncached)
    cache.occupancy.entries shouldBe 3               // film 1 reused, film 2 rendered twice
  }

  it should "render a card again when only its metadata moves — a rating, not a showtime" in {
    val cache = new CaffeineFilmCardFragments(CaffeineFilmCardFragments.DefaultMaxBytes)
    listing(Seq(film(1)), cache)
    val rerated = Seq(film(1, imdb = 8.4))
    listing(rerated, cache) shouldBe listing(rerated, FilmCardFragments.Uncached)
    cache.occupancy.entries shouldBe 2
  }

  private val threads = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
  private def allocatedBy(f: => Any): Long = {
    val id = Thread.currentThread.threadId
    val before = threads.getThreadAllocatedBytes(id); f
    threads.getThreadAllocatedBytes(id) - before
  }

  /** The cards written gzipped, as a listing's body goes out — what the cache replaces
   *  is their rendering; the bytes still have to be written. */
  private def cardsBody(films: Seq[FilmSchedule], fragments: FilmCardFragments) =
    controllers.ResponseBody.html(views.html._filmCards(films)(using city, summon, fragments)).gzipped

  it should "write an unchanged film's card for a fraction of what rendering it costs" in {
    val films = (0 until 150).map(film(_))
    val cache = new CaffeineFilmCardFragments(CaffeineFilmCardFragments.DefaultMaxBytes)
    for (_ <- 1 to 3) { cardsBody(films, FilmCardFragments.Uncached); cardsBody(films, cache) }   // warm, and fill
    val rendered = allocatedBy(cardsBody(films, FilmCardFragments.Uncached))
    val reused   = allocatedBy(cardsBody(films, cache))
    withClue(s"rendered ${rendered / 1024} KB, from the cache ${reused / 1024} KB: ")(reused should be < rendered / 4)
  }

  // A Java string holds one byte per char until its first char outside Latin-1, then two
  // for all of it — and a kept card is that string. An English card's one such char was
  // the hide button's literal ✕, which doubled every card the cache holds on a
  // non-Polish host; the entity renders the same glyph.
  it should "hold an English card at one byte per char" in {
    val london   = City.bySlug("london").getOrElse(fail("no city 'london'"))
    val venues   = Cinema.all.distinct.filter(c => City.forCinema(c).contains(london)).take(6)
    val card = views.html._filmCards(Seq(film(1, cinemas = venues)))(using london, testsupport.TestMessages.forLang("en"),
      FilmCardFragments.Uncached).body
    card.filter(_ > 0xFF).distinct.map(c => f"U+${c.toInt}%04X") shouldBe empty
  }
}

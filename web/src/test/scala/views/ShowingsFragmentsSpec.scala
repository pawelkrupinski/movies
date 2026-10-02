package views

import testsupport.TestMessages.given

import controllers.{CaffeineShowingsFragments, CinemaShowtimes, FilmSchedule, ShowingsFragments}
import models.{Cinema, City, Movie, MovieRecord, Showtime}
import services.readmodel.TestReadModel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.lang.management.ManagementFactory
import java.time.LocalDate

/**
 * A listing re-rendered with its films' showings unchanged writes them from
 * [[CaffeineShowingsFragments]] instead of rendering them again — byte for byte the
 * page an uncached render produces, and a different page the moment a showtime moves.
 */
class ShowingsFragmentsSpec extends AnyFlatSpec with Matchers {

  private implicit val city: City = City.bySlug("poznan").getOrElse(fail("no city 'poznan'"))
  private val day = LocalDate.of(2026, 5, 13)
  private val cinemas = Cinema.all.distinct.filter(c => City.forCinema(c).contains(city)).take(6)

  private def film(n: Int, firstHour: Int = 10): FilmSchedule = FilmSchedule(
    movie = Movie(s"Film $n", Some(110)), posterUrl = None, synopsis = None, cast = Nil, director = Nil,
    cinemaFilmUrls = cinemas.take(2).map(c => c -> s"https://kino.example/film-$n"),
    showings = (0 until 4).map { d =>
      val date = day.plusDays(d.toLong)
      date -> cinemas.map(c => CinemaShowtimes(c, (0 until 5).map(s =>
        Showtime(date.atTime(firstHour + s * 2, 15), Some(s"https://kino.example/book?film=$n&date=$date&id=${n * 100 + s}"), None, Nil))))
    },
    resolved = TestReadModel.resolved(s"Film $n", Some(2026), MovieRecord()),
    slug = Some(s"film-$n"), asOf = day)

  private def listing(films: Seq[FilmSchedule])(using fragments: ShowingsFragments): String =
    views.html._filmCards(films).body

  "a listing rendered through the fragment cache" should "be byte for byte the uncached page" in {
    val films = (0 until 3).map(film(_))
    val uncached = listing(films)(using ShowingsFragments.Uncached)
    val cache = new CaffeineShowingsFragments(CaffeineShowingsFragments.DefaultMaxBytes)
    listing(films)(using cache) shouldBe uncached
    listing(films)(using cache) shouldBe uncached     // ...and again, from the cache
    cache.occupancy.hitRatio.get shouldBe 0.5
  }

  it should "render a film again the moment its showings change, and only that film" in {
    val cache = new CaffeineShowingsFragments(CaffeineShowingsFragments.DefaultMaxBytes)
    val before = listing(Seq(film(1), film(2)))(using cache)
    val moved  = Seq(film(1), film(2, firstHour = 11))
    val after  = listing(moved)(using cache)
    after should not be before
    after shouldBe listing(moved)(using ShowingsFragments.Uncached)
    cache.occupancy.entries shouldBe 3                // film 1 reused, film 2 rendered twice
  }

  private val threads = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
  private def allocatedBy(f: => Any): Long = {
    val id = Thread.currentThread.threadId
    val before = threads.getThreadAllocatedBytes(id); f
    threads.getThreadAllocatedBytes(id) - before
  }

  /** The films' showings trees, written gzipped as a listing's body goes out — what
   *  the cache replaces is their rendering; the bytes still have to be written. */
  private def showingsBody(films: Seq[FilmSchedule], fragments: ShowingsFragments) =
    controllers.ResponseBody.html(new play.twirl.api.Html(films.map(f => views.html._filmShowings(f)(using city, fragments)))).gzipped

  it should "write an unchanged film's showings for a fraction of what rendering them costs" in {
    val films = (0 until 150).map(film(_))
    val cache = new CaffeineShowingsFragments(CaffeineShowingsFragments.DefaultMaxBytes)
    for (_ <- 1 to 3) { showingsBody(films, ShowingsFragments.Uncached); showingsBody(films, cache) }   // warm, and fill
    val rendered = allocatedBy(showingsBody(films, ShowingsFragments.Uncached))
    val reused   = allocatedBy(showingsBody(films, cache))
    withClue(s"rendered ${rendered / 1024} KB, from the cache ${reused / 1024} KB: ")(reused should be < rendered / 3)
  }
}

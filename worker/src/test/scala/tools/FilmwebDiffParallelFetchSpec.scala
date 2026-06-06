package tools

import models.{Cinema, CinemaMovie, KinoMikro, KinoNaBoku, Movie}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.CinemaScraper

import java.util.concurrent.atomic.AtomicInteger
import scala.util.{Failure, Success}

/** Pins `FilmwebDiff.fetchOursInParallel`: parallel pre-fetch of the OUR side
 *  must (1) run every scraper, (2) key each result to its OWN cinema (no
 *  cross-attribution under concurrency), and (3) `Try`-isolate a throwing
 *  scraper so one bad cinema can't sink the batch. */
class FilmwebDiffParallelFetchSpec extends AnyFlatSpec with Matchers {

  private def movieFor(c: Cinema): CinemaMovie =
    CinemaMovie(Movie(c.displayName + " film"), c, None, None, None, Nil, Nil, Nil)

  private class FakeScraper(override val cinema: Cinema, calls: AtomicInteger, boom: Boolean = false)
    extends CinemaScraper {
    def fetch(): Seq[CinemaMovie] = {
      calls.incrementAndGet()
      Thread.sleep(20) // overlap windows so a keying bug would surface
      if (boom) throw new RuntimeException("scrape blew up")
      Seq(movieFor(cinema))
    }
  }

  "fetchOursInParallel" should "run every scraper and key each result to its own cinema" in {
    val calls = new AtomicInteger(0)
    val scrapers = Seq(new FakeScraper(KinoNaBoku, calls), new FakeScraper(KinoMikro, calls))

    val results = FilmwebDiff.fetchOursInParallel(scrapers)

    calls.get() shouldBe 2
    results.keySet shouldBe Set(KinoNaBoku, KinoMikro)
    results(KinoNaBoku) match {
      case Success(ms) => ms.map(_.cinema).toSet shouldBe Set(KinoNaBoku)
      case Failure(e)  => fail(s"unexpected failure: $e")
    }
    results(KinoMikro).get.map(_.cinema).toSet shouldBe Set(KinoMikro)
  }

  it should "isolate a throwing scraper without sinking the batch" in {
    val calls = new AtomicInteger(0)
    val scrapers = Seq(new FakeScraper(KinoNaBoku, calls, boom = true), new FakeScraper(KinoMikro, calls))

    val results = FilmwebDiff.fetchOursInParallel(scrapers)

    results(KinoNaBoku).isFailure shouldBe true
    results(KinoMikro).get.map(_.cinema).toSet shouldBe Set(KinoMikro)
  }

  it should "return an empty map for no scrapers" in {
    FilmwebDiff.fetchOursInParallel(Nil) shouldBe empty
  }
}

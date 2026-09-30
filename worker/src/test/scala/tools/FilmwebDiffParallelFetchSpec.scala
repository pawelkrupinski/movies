package tools

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import models.{Cinema, CinemaMovie, KinoMikro, KinoNaBoku, Movie}
import services.cinemas.StubCinemaScraper

import scala.util.{Failure, Success}

/** Pins `FilmwebDiff.fetchOursInParallel`: parallel pre-fetch of the OUR side
 *  must (1) run every scraper, (2) key each result to its OWN cinema (no
 *  cross-attribution under concurrency), and (3) `Try`-isolate a throwing
 *  scraper so one bad cinema can't sink the batch. */
class FilmwebDiffParallelFetchSpec extends AnyFlatSpec with Matchers {

  private def movieFor(c: Cinema): CinemaMovie =
    CinemaMovie(Movie(c.displayName + " film"), c, None, None, None, Nil, Nil, Nil)

  /** Sleeps so the parallel fetches overlap — a keying bug would surface. */
  private def scraper(c: Cinema, boom: Boolean = false) = new StubCinemaScraper(c, {
    Thread.sleep(20)
    if (boom) throw new RuntimeException("scrape blew up")
    Seq(movieFor(c))
  })

  "fetchOursInParallel" should "run every scraper and key each result to its own cinema" in {
    val scrapers = Seq(scraper(KinoNaBoku), scraper(KinoMikro))

    val results = FilmwebDiff.fetchOursInParallel(scrapers)

    scrapers.map(_.calls) shouldBe Seq(1, 1)
    results.keySet shouldBe Set(KinoNaBoku, KinoMikro)
    results(KinoNaBoku) match {
      case Success(ms) => ms.map(_.cinema).toSet shouldBe Set(KinoNaBoku)
      case Failure(e)  => fail(s"unexpected failure: $e")
    }
    results(KinoMikro).get.map(_.cinema).toSet shouldBe Set(KinoMikro)
  }

  it should "isolate a throwing scraper without sinking the batch" in {
    val scrapers = Seq(scraper(KinoNaBoku, boom = true), scraper(KinoMikro))

    val results = FilmwebDiff.fetchOursInParallel(scrapers)

    results(KinoNaBoku).isFailure shouldBe true
    results(KinoMikro).get.map(_.cinema).toSet shouldBe Set(KinoMikro)
  }

  it should "return an empty map for no scrapers" in {
    FilmwebDiff.fetchOursInParallel(Nil) shouldBe empty
  }
}

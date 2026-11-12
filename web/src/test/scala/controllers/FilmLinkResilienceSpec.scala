package controllers

import models.{MovieRecord, Poznan, Rialto, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ch.qos.logback.classic.Level
import services.readmodel.TestReadModel
import tools.LogCapture

import java.time.LocalDateTime

/** A film deep-link must survive the sub-second window where the worker
 *  re-projects or re-keys a film and the `toSchedules` join momentarily drops it
 *  (its `web_movies` + `web_screenings` documents arrive over two independent
 *  change streams). Modelled here by a film the read model knows whose only
 *  showtimes are in the past, so the live join yields nothing while the movie
 *  document is still present — the same shape as an in-flight reprojection (and
 *  as a genuinely-ended run). The lookup now falls back to the movie document
 *  and renders an empty-showings page rather than a 404. */
class FilmLinkResilienceSpec extends AnyFlatSpec with Matchers {

  private val past   = TestCityControllers.now.minusDays(2)
  private val future = TestCityControllers.now.plusDays(2)

  private def showing(title: String, year: Int, showAt: LocalDateTime) =
    (title, Some(year), MovieRecord(data = Map[Source, SourceData](
      Rialto -> SourceData(title = Some(title), showtimes = Seq(Showtime(showAt, bookingUrl = None))))))

  private def serviceOver(rows: (String, Option[Int], MovieRecord)*) =
    new MovieControllerService(TestReadModel.fromRecords(rows), clock = TestCityControllers.clock)

  private def serviceWith(showAt: LocalDateTime) = serviceOver(showing("Drugie życie", 2026, showAt))

  /** The fallback's log lines, as (level, message). */
  private def fallbackLog(body: => Unit): Seq[(Level, String)] =
    LogCapture.capture(classOf[MovieControllerService].getName, Some(Level.INFO))(body)
      .filter(_.getFormattedMessage.startsWith("film deep-link"))
      .map(e => e.getLevel -> e.getFormattedMessage)

  "film" should "still resolve a known film with no live schedule (reprojection/rekey gap or ended run) instead of 404ing" in {
    val schedule = serviceWith(past).film(Poznan, "Drugie życie")
    schedule.map(_.movie.title) shouldBe Some("Drugie życie")
    schedule.map(_.showings)    shouldBe Some(Seq.empty)
  }

  it should "prefer the live schedule with its showtimes when the film is currently playing" in {
    serviceWith(future).film(Poznan, "Drugie życie").map(_.showings.nonEmpty) shouldBe Some(true)
  }

  it should "still return None for a title the read model has never seen" in {
    serviceWith(past).film(Poznan, "Nie ma takiego filmu") shouldBe None
  }

  // ~2,000 such hits a day in prod (2026-11-12 census) were crawlers and old links to
  // ended runs; as a WARN they buried the one case that is a fault.
  "an ended run" should "log at INFO, not WARN: it is crawler and old-link traffic, not a gap" in {
    val log = fallbackLog(serviceWith(past).filmBySlug(Poznan, "drugie-zycie") shouldBe defined)
    log.map(_._1) shouldBe Seq(Level.INFO)
    log.head._2 should include("slug='drugie-zycie'")
  }

  "an address whose film has nothing here while a namesake plays here" should "WARN, naming both films" in {
    // One of a same-title pair holds the bare slug (`FilmSlugs`' order puts `padasnieg|2025`
    // first); it ended here, its namesake is live here — a link left on the dead row.
    val service = serviceOver(showing("Pada śnieg", 2025, past), showing("Pada śnieg", 2026, future))
    val log     = fallbackLog(service.filmBySlug(Poznan, "pada-snieg").map(_.showings) shouldBe Some(Seq.empty))
    log.map(_._1) shouldBe Seq(Level.WARN)
    log.head._2 should (include("slug='pada-snieg'") and include("filmId=padasnieg|2025") and include("namesake=padasnieg|2026"))
  }
}

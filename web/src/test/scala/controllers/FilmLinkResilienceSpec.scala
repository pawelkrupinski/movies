package controllers

import models.{MovieRecord, Poznan, Rialto, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.readmodel.TestReadModel

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

  private val past   = LocalDateTime.now().minusDays(2)
  private val future = LocalDateTime.now().plusDays(2)

  private def serviceWith(showAt: LocalDateTime) = new MovieControllerService(
    TestReadModel.fromRecords(Seq(("Drugie życie", Some(2026), MovieRecord(data = Map[Source, SourceData](
      Rialto -> SourceData(title = Some("Drugie życie"),
                           showtimes = Seq(Showtime(showAt, bookingUrl = None)))
    ))))))

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
}

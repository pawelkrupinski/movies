package services.identity
package adapters

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.MutableClock

import java.time.Instant

/** The model's input gate over the families (docs/design/identity-structural-fixes.md fix 1): a listing waits on each of
 *  its own questions until the question memory holds an attempt of it -- answered, a durable non-answer, or a failure
 *  (waiting for a failure to be given up on held listings for hours in prod, 2026-11-06); and asks them. */
class AgreementReadinessSpec extends AnyFlatSpec with Matchers {
  private val title  = Question.Family(agreement.VoterFamily.Wiki, "title|Geister")
  private val record = Question.Family(agreement.VoterFamily.Imdb, "record|tt1")

  "a listing's readiness" should "wait on each of its questions until it was attempted once, never until given up, asking only what it waits on" in {
    val clock   = new MutableClock(Instant.parse("2026-11-06T10:00:00Z"))
    val memory  = new QuestionMemory(new InMemoryTmdbDocuments, "de-DE", clock)
    val asked   = scala.collection.mutable.ArrayBuffer.empty[Set[Question]]
    val ready   = new AgreementReadiness(_ => Set(title, record), memory.attempted, asked += _)
    val geister = ModelRead.corpus(Seq(models.KinoMuza -> Seq(models.CinemaMovie(models.Movie("Geister"), models.KinoMuza, None, None, None,
      Nil, Nil, Seq(models.Showtime(java.time.LocalDateTime.of(2026, 11, 6, 18, 0), None))))), services.movies.SingleCountryNormalizer.titleNormalizer).head
    ready.awaiting(geister) shouldBe Set(title, record)                    // never asked
    memory.failed(Seq(title))
    ready.awaiting(geister) shouldBe Set(record)                           // failed once: attempted, retried all the same
    memory.unanswered(Seq(record))
    ready.awaiting(geister) shouldBe empty                                 // a durable non-answer
    ready.ask(geister, Set.empty); ready.ask(geister, Set(record))
    asked.toSeq shouldBe Seq(Set(record))
  }
}

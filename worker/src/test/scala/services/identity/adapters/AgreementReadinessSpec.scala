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
  private val geister = ModelRead.corpus(Seq(models.KinoMuza -> Seq(models.CinemaMovie(models.Movie("Geister"), models.KinoMuza, None, None, None,
    Nil, Nil, Seq(models.Showtime(java.time.LocalDateTime.of(2026, 11, 6, 18, 0), None))))), services.movies.SingleCountryNormalizer.titleNormalizer).head

  "a listing's readiness" should "wait on each of its questions until it was attempted once, never until given up, asking only what it waits on" in {
    val clock   = new MutableClock(Instant.parse("2026-11-06T10:00:00Z"))
    val memory  = new QuestionMemory(new InMemoryTmdbDocuments, "de-DE", clock)
    val asked   = scala.collection.mutable.ArrayBuffer.empty[Set[Question]]
    val ready   = new AgreementReadiness(_ => Set(title, record), memory.attempted, asked += _)
    ready.awaiting(geister) shouldBe Set(title, record)                    // never asked
    memory.failed(Seq(title))
    ready.awaiting(geister) shouldBe Set(record)                           // failed once: attempted, retried all the same
    memory.unanswered(Seq(record))
    ready.awaiting(geister) shouldBe empty                                 // a durable non-answer
    ready.ask(geister, Set.empty); ready.ask(geister, Set(record))
    asked.toSeq shouldBe Seq(Set(record))
  }

  // A design choice, pinned (the owner, 2026-11-07): an ask a host's open breaker or the fleet's pace turned away is an
  // attempt the gate takes the listing in on, never a wait it holds it for -- held while a source is broken, every new
  // version of its listings would stall until the source came back (or the 10-minute bound took them in)
  it should "take a listing in once its question was turned away by a host's open breaker, no failure counted" in {
    val clock   = new MutableClock(Instant.parse("2026-11-06T10:00:00Z"))
    val memory  = new QuestionMemory(new InMemoryTmdbDocuments, "de-DE", clock)
    val ready   = new AgreementReadiness(_ => Set(title), memory.attempted, _ => ())
    ready.awaiting(geister) shouldBe Set(title)
    memory.turnedAway(Seq(title), Some("www.wikidata.org")) shouldBe Map(title -> 0)
    ready.awaiting(geister) shouldBe empty                                 // attempted: taken in, Pending on it as before
    memory.failedPastGiveUp(title) shouldBe false                          // and not given up on: asked again on its backoff
  }
}

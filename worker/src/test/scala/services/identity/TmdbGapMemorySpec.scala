package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.MutableClock

import java.time.{Duration, Instant}
import scala.jdk.DurationConverters._

/** A gap TMDB left unanswered is asked again a day later, not every round — and only the fill backs off. */
class TmdbGapMemorySpec extends AnyFlatSpec with Matchers {
  "the fill's gap memory" should "hold back a question left unanswered for a day, then offer it again" in {
    val clock  = new MutableClock(Instant.parse("2026-09-28T12:00:00Z"))
    val memory = new TmdbGapMemory(new InMemoryTmdbDocuments, "pl-PL", clock)
    val (lalka, rosa) = (CandidateQuery.Title("Lalka"), CandidateQuery.Title("Róża"))
    val gaps = AnswersChanged(Set(lalka, rosa), Set(1018, 9))
    memory.unanswered(Seq(lalka), Seq(1018))
    memory.due(gaps) shouldBe AnswersChanged(Set(rosa), Set(9))
    clock.advance(Duration.ofHours(23))
    memory.due(gaps) shouldBe AnswersChanged(Set(rosa), Set(9))
    clock.advance(Duration.ofHours(2))
    memory.due(gaps) shouldBe gaps
  }

  // A question that FAILED (5xx, timeout, 429) learned nothing about the film: it waited the same
  // day as one TMDB answered "nothing here", so a ten-minute TMDB outage held a round's worth of
  // the model's questions back for 24 hours. A failure is asked again in minutes, doubling, capped.
  it should "ask a question that failed again within minutes, doubling the wait while it keeps failing" in {
    val clock  = new MutableClock(Instant.parse("2026-09-28T12:00:00Z"))
    val memory = new TmdbGapMemory(new InMemoryTmdbDocuments, "pl-PL", clock)
    val lalka  = CandidateQuery.Title("Lalka")
    val gaps   = AnswersChanged(Set(lalka), Set(1018))
    memory.failed(Seq(lalka), Seq(1018))
    memory.due(gaps) shouldBe AnswersChanged.Empty
    clock.advance(TmdbGapMemory.FirstFailureRetry.toJava.plusSeconds(1))
    memory.due(gaps) shouldBe gaps                                   // minutes, not a day

    memory.failed(Seq(lalka), Seq(1018))                             // failed again: twice the wait
    clock.advance(TmdbGapMemory.FirstFailureRetry.toJava.plusSeconds(1))
    memory.due(gaps) shouldBe AnswersChanged.Empty
    clock.advance(TmdbGapMemory.FirstFailureRetry.toJava)
    memory.due(gaps) shouldBe gaps

    (1 to 30).foreach(_ => memory.failed(Seq(lalka), Seq(1018)))     // capped, however long it fails
    clock.advance(TmdbGapMemory.MaxFailureRetry.toJava.plusSeconds(1))
    memory.due(gaps) shouldBe gaps
  }

  it should "give a durable answer the day's wait even after a run of failures" in {
    val clock  = new MutableClock(Instant.parse("2026-09-28T12:00:00Z"))
    val memory = new TmdbGapMemory(new InMemoryTmdbDocuments, "pl-PL", clock)
    val lalka  = CandidateQuery.Title("Lalka")
    val gaps   = AnswersChanged(Set(lalka), Set.empty)
    memory.failed(Seq(lalka), Nil)
    memory.unanswered(Seq(lalka), Nil)
    clock.advance(Duration.ofHours(23))
    memory.due(gaps) shouldBe AnswersChanged.Empty
  }

  // Per-question backoff alone left every question of a long TMDB outage up to six hours behind a
  // TMDB answering again. The first clean round after an all-failed one makes them due at once.
  it should "offer every question failed during an outage again as soon as a round finds TMDB answering" in {
    val clock  = new MutableClock(Instant.parse("2026-09-28T12:00:00Z"))
    val memory = new TmdbGapMemory(new InMemoryTmdbDocuments, "pl-PL", clock)
    val lalka  = CandidateQuery.Title("Lalka")
    val gaps   = AnswersChanged(Set(lalka), Set(1018))
    (1 to 10).foreach { _ => memory.failed(Seq(lalka), Seq(1018)); memory.roundEnded(answered = 0, failed = 1, deferred = 4) }
    clock.advance(Duration.ofMinutes(1))
    memory.due(gaps) shouldBe AnswersChanged.Empty                   // six hours of backoff
    memory.roundEnded(answered = 3, failed = 0, deferred = 0)                      // TMDB answers again
    memory.due(gaps) shouldBe gaps
  }

  it should "keep a question's own backoff when it fails while other reads answer" in {
    val clock  = new MutableClock(Instant.parse("2026-09-28T12:00:00Z"))
    val memory = new TmdbGapMemory(new InMemoryTmdbDocuments, "pl-PL", clock)
    val lalka  = CandidateQuery.Title("Lalka")
    val gaps   = AnswersChanged(Set(lalka), Set.empty)
    memory.failed(Seq(lalka), Nil); memory.roundEnded(answered = 5, failed = 1, deferred = 0)
    clock.advance(Duration.ofMinutes(1))
    memory.roundEnded(answered = 5, failed = 0, deferred = 0)
    memory.due(gaps) shouldBe AnswersChanged.Empty
  }

  // A round asking ONE question that fails is that question's failure, not TMDB down: marked an
  // outage, the next clean round released it, and a persistently failing question was retried every
  // other round instead of backing off to six hours.
  it should "keep a lone failed read's own backoff: too few reads to call an outage" in {
    val clock  = new MutableClock(Instant.parse("2026-09-28T12:00:00Z"))
    val memory = new TmdbGapMemory(new InMemoryTmdbDocuments, "pl-PL", clock)
    val lalka  = CandidateQuery.Title("Lalka")
    val gaps   = AnswersChanged(Set(lalka), Set.empty)
    memory.failed(Seq(lalka), Nil); memory.roundEnded(answered = 0, failed = 1, deferred = 0)
    clock.advance(Duration.ofMinutes(1))
    memory.roundEnded(answered = 5, failed = 0, deferred = 0)
    memory.due(gaps) shouldBe AnswersChanged.Empty
  }
}

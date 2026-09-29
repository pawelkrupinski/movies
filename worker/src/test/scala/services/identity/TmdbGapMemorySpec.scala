package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.MutableClock

import java.time.{Duration, Instant}

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
}

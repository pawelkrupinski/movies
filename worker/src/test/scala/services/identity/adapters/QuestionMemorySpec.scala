package services.identity
package adapters

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.agreement.AgreementStage.PosterQuestion
import services.identity.agreement.VoterFamily
import tools.MutableClock

import java.time.{Duration, Instant}
import scala.jdk.DurationConverters._

/** A question its source left unanswered is asked again after its kind's wait, not every round — whoever asks it, the
 *  model's fill and live asks or the agreement's asker — and only the asking backs off. */
class QuestionMemorySpec extends AnyFlatSpec with Matchers {
  private def memory(clock: MutableClock, docs: InMemoryTmdbDocuments = new InMemoryTmdbDocuments) = new QuestionMemory(docs, "pl-PL", clock)
  private def at = new MutableClock(Instant.parse("2026-09-28T12:00:00Z"))
  private val (lalka, rosa) = (CandidateQuery.Title("Lalka"), CandidateQuery.Title("Róża"))
  private val imdbLalka     = Question.Family(VoterFamily.Imdb, "title|Lalka")

  "the memory" should "hold back a model question left unanswered for a day, then offer it again" in {
    val clock  = at
    val memory = this.memory(clock)
    val gaps   = AnswersChanged(Set(lalka, rosa), Set(1018, 9))
    memory.unanswered(Seq(Question.Candidate(lalka), Question.FilmRecord(1018)))
    memory.due(gaps) shouldBe AnswersChanged(Set(rosa), Set(9))
    clock.advance(Duration.ofHours(23))
    memory.due(gaps) shouldBe AnswersChanged(Set(rosa), Set(9))
    clock.advance(Duration.ofHours(2))
    memory.due(gaps) shouldBe gaps
  }

  // Markers live in their own collection since 2026-10-24; one left in tmdb_queries from before still holds its question
  // back until the sweep's grace deletes it, so the move asks nothing again early.
  it should "keep its markers apart from the answers, under the ids they had, and still honour one kept with them before the move" in {
    val clock  = at
    val docs   = new InMemoryTmdbDocuments
    val memory = this.memory(clock, docs)
    memory.unanswered(Seq(Question.Candidate(lalka), Question.FilmRecord(1018)))
    docs.fetchedBefore(TmdbKind.Marker, Long.MaxValue).map(_._1).toSet shouldBe
      Set(s"unanswered|${TmdbStore.questionId("pl-PL", lalka)}", "unanswered|film|1018")
    docs.size(TmdbKind.Query) shouldBe 0
    val old = docs.get(TmdbKind.Marker, Seq("unanswered|film|1018")).head._2
    docs.put(TmdbKind.Query, Seq(s"${QuestionMemory.Prefix}${TmdbStore.questionId("pl-PL", rosa)}" -> old))
    memory.due(AnswersChanged(Set(lalka, rosa), Set.empty)) shouldBe AnswersChanged.Empty
  }

  // A question that FAILED (5xx, timeout, 429) learned nothing about the film: it waited the same
  // day as one TMDB answered "nothing here", so a ten-minute TMDB outage held a round's worth of
  // the model's questions back for 24 hours. A failure is asked again in minutes, doubling, capped.
  it should "ask a question that failed again within minutes, doubling the wait while it keeps failing" in {
    val clock  = at
    val memory = this.memory(clock)
    val gaps   = AnswersChanged(Set(lalka), Set(1018))
    val asked  = Seq(Question.Candidate(lalka), Question.FilmRecord(1018))
    memory.failed(asked)
    memory.due(gaps) shouldBe AnswersChanged.Empty
    clock.advance(QuestionPolicies.FirstFailureWait.toJava.plusSeconds(1))
    memory.due(gaps) shouldBe gaps                                   // minutes, not a day

    memory.failed(asked)                                             // failed again: twice the wait
    clock.advance(QuestionPolicies.FirstFailureWait.toJava.plusSeconds(1))
    memory.due(gaps) shouldBe AnswersChanged.Empty
    clock.advance(QuestionPolicies.FirstFailureWait.toJava)
    memory.due(gaps) shouldBe gaps

    (1 to 30).foreach(_ => memory.failed(asked))                     // capped, however long it fails
    clock.advance(QuestionPolicies.MaxFailureWait.toJava.plusSeconds(1))
    memory.due(gaps) shouldBe gaps
  }

  it should "give a durable answer the day's wait even after a run of failures, and count failures afresh after it" in {
    val clock  = at
    val memory = this.memory(clock)
    val gaps   = AnswersChanged(Set(lalka), Set.empty)
    memory.failed(Seq(Question.Candidate(lalka))) shouldBe Map(Question.Candidate(lalka) -> 1)
    memory.failed(Seq(Question.Candidate(lalka))) shouldBe Map(Question.Candidate(lalka) -> 2)
    memory.unanswered(Seq(Question.Candidate(lalka)))
    clock.advance(Duration.ofHours(23))
    memory.due(gaps) shouldBe AnswersChanged.Empty
    memory.failed(Seq(Question.Candidate(lalka))) shouldBe Map(Question.Candidate(lalka) -> 1)
  }

  // Per-question backoff alone left every question of a long TMDB outage up to six hours behind a
  // TMDB answering again. The first clean round after an all-failed one makes them due at once.
  it should "offer every model question failed during an outage again as soon as a round finds TMDB answering" in {
    val clock  = at
    val memory = this.memory(clock)
    val gaps   = AnswersChanged(Set(lalka), Set(1018))
    (1 to 10).foreach { _ =>
      memory.failed(Seq(Question.Candidate(lalka), Question.FilmRecord(1018), imdbLalka))
      memory.roundEnded(answered = 0, failed = 1, deferred = 4)
    }
    clock.advance(Duration.ofMinutes(1))
    memory.due(gaps) shouldBe AnswersChanged.Empty                   // six hours of backoff
    memory.roundEnded(answered = 3, failed = 0, deferred = 0)        // TMDB answers again
    memory.due(gaps) shouldBe gaps
    memory.due(Set[Question](imdbLalka)) shouldBe empty              // TMDB's recovery says nothing of IMDb
  }

  it should "keep a question's own backoff when it fails while other reads answer" in {
    val clock  = at
    val memory = this.memory(clock)
    val gaps   = AnswersChanged(Set(lalka), Set.empty)
    memory.failed(Seq(Question.Candidate(lalka))); memory.roundEnded(answered = 5, failed = 1, deferred = 0)
    clock.advance(Duration.ofMinutes(1))
    memory.roundEnded(answered = 5, failed = 0, deferred = 0)
    memory.due(gaps) shouldBe AnswersChanged.Empty
  }

  // A round asking ONE question that fails is that question's failure, not TMDB down: marked an
  // outage, the next clean round released it, and a persistently failing question was retried every
  // other round instead of backing off to six hours.
  it should "keep a lone failed read's own backoff: too few reads to call an outage" in {
    val clock  = at
    val memory = this.memory(clock)
    val gaps   = AnswersChanged(Set(lalka), Set.empty)
    memory.failed(Seq(Question.Candidate(lalka))); memory.roundEnded(answered = 0, failed = 1, deferred = 0)
    clock.advance(Duration.ofMinutes(1))
    memory.roundEnded(answered = 5, failed = 0, deferred = 0)
    memory.due(gaps) shouldBe AnswersChanged.Empty
  }

  // Until C3 an agreement question that failed was asked again on the task queue's backoff, its own memory apart.
  it should "remember an agreement question's failure under its id, on the same backoff, and nothing of an empty answer" in {
    val clock  = at
    val docs   = new InMemoryTmdbDocuments
    val memory = this.memory(clock, docs)
    val find   = Question.Find("tt0063208")
    memory.failed(Seq(imdbLalka, find))
    docs.fetchedBefore(TmdbKind.Marker, Long.MaxValue).map(_._1).toSet shouldBe Set("unanswered|imdb|title|Lalka", "unanswered|find|tt0063208")
    memory.due(Set[Question](imdbLalka, find)) shouldBe empty
    clock.advance(QuestionPolicies.FirstFailureWait.toJava.plusSeconds(1))
    memory.due(Set[Question](imdbLalka, find)) shouldBe Set(imdbLalka, find)
    memory.unanswered(Seq(imdbLalka))                                 // an empty answer is the store's to file
    memory.due(Set[Question](imdbLalka)) shouldBe Set(imdbLalka)
  }

  it should "hold a poster given up on back for the unread wait, and pass what it never remembers" in {
    val clock  = at
    val memory = this.memory(clock)
    val poster = Question.Poster(PosterQuestion.Venue("https://example.org/p.jpg"))
    memory.unanswered(Seq(poster))
    clock.advance(QuestionPolicies.PosterUnreadAge.toJava.minusMinutes(1))
    memory.due(Set[Question](poster)) shouldBe empty
    clock.advance(Duration.ofMinutes(2))
    memory.due(Set[Question](poster)) shouldBe Set(poster)
    val unremembered = Set[Question](Question.Person(7), Question.VenuePage(services.movies.ListingKey.Native("kino-muranow", "42", "Lalka")))
    memory.failed(unremembered) shouldBe empty
    memory.due(unremembered) shouldBe unremembered
  }
}

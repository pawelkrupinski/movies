package services.identity
package adapters

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.MutableClock

import java.time.{Duration, Instant}

/** The model asks live what is new, at once; a gap it already asked and was left without waits out its backoff, the
 *  fill's to ask (docs/design/identity-questions.md, step 3). */
class LiveAsksSpec extends AnyFlatSpec with Matchers {
  private val lalka = CandidateQuery.Title("Lalka")

  private final class Live(answer: => Answer[Seq[Hit]], fail: Boolean = false) extends IdentityLookups {
    var asked = 0
    def hasDetail(listing: Listing): Boolean                      = false
    def detail(listing: Listing): Answer[Option[DetailFacts]]     = Answer.Known(None)
    def candidates(q: CandidateQuery): Answer[Seq[Hit]] = {
      asked += 1
      // a read that fails counts on this thread, through the counted fetch the live lookups ask
      if (fail) scala.util.Try(new LiveAsks.Counted(new tools.HttpFetch {
        def get(url: String): String = throw new java.net.SocketTimeoutException("slow")
        def post(url: String, body: String, contentType: String): String = get(url)
      }).get("https://api.themoviedb.org/3/x"))
      answer
    }
    def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = Answer.Known(None)
  }
  private val nothingStored = new IdentityLookups {
    def hasDetail(listing: Listing): Boolean                      = false
    def detail(listing: Listing): Answer[Option[DetailFacts]]     = Answer.Known(None)
    def candidates(q: CandidateQuery): Answer[Seq[Hit]]          = Answer.Unknown
    def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = Answer.Unknown
  }

  "the model's live asks" should "ask a new question at once, then leave one left unanswered to the fill for a day" in {
    val clock   = new MutableClock(Instant.parse("2026-10-24T12:00:00Z"))
    val memory  = new QuestionMemory(new InMemoryTmdbDocuments, "pl-PL", clock)
    val live    = new Live(Answer.Unknown)
    val lookups = new StoredFirstLookups(nothingStored, live, LiveAsks.remembered(memory))
    lookups.candidates(lalka) shouldBe Answer.Unknown
    lookups.candidates(lalka) shouldBe Answer.Unknown
    live.asked shouldBe 1                                                     // asked once, then the gap it is
    memory.due(AnswersChanged(Set(lalka), Set.empty)) shouldBe AnswersChanged.Empty
    clock.advance(Duration.ofHours(25))
    lookups.candidates(lalka)
    live.asked shouldBe 2
  }

  it should "remember a read that failed on the failure backoff, minutes not a day" in {
    val clock   = new MutableClock(Instant.parse("2026-10-24T12:00:00Z"))
    val memory  = new QuestionMemory(new InMemoryTmdbDocuments, "pl-PL", clock)
    val live    = new Live(Answer.Unknown, fail = true)
    val lookups = new StoredFirstLookups(nothingStored, live, LiveAsks.remembered(memory))
    lookups.candidates(lalka)
    lookups.candidates(lalka)
    live.asked shouldBe 1
    clock.advance(Duration.ofMinutes(6))
    lookups.candidates(lalka)
    live.asked shouldBe 2
  }

  it should "remember nothing of a question answered, and ask every gap without a memory" in {
    val clock    = new MutableClock(Instant.parse("2026-10-24T12:00:00Z"))
    val docs     = new InMemoryTmdbDocuments
    val answered = new Live(Answer.Known(Nil))
    new StoredFirstLookups(nothingStored, answered, LiveAsks.remembered(new QuestionMemory(docs, "pl-PL", clock))).candidates(lalka) shouldBe Answer.Known(Nil)
    docs.size(TmdbKind.Marker) shouldBe 0
    val always = new Live(Answer.Unknown)
    val plain  = new StoredFirstLookups(nothingStored, always)
    plain.candidates(lalka); plain.candidates(lalka)
    always.asked shouldBe 2
  }
}

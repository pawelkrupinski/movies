package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity._

/** identity-slice --faithfulness: why this code's model decides a captured listing otherwise — the questions it left
 *  unanswered, and whether the capture could have held them ([[IdentitySlice.modelMovedWhy]], [[IdentitySlice.Asking]]). */
class IdentitySliceFaithfulnessSpec extends AnyFlatSpec with Matchers {

  private val de = models.Country.all.find(_.code == "de").get
  private val noLive   = tools.FiledAnswers.of(de)
  private val withLive = tools.FiledAnswers(noLive.capture, "de.json.gz", Map("GET https://v3.sg.media-imdb.com/suggestion/w/when.json" -> "{}"))

  /** Answers one search, gaps every other question. */
  private val recording = new IdentityLookups {
    def hasDetail(l: Listing): Boolean = false
    def detail(l: Listing): Answer[Option[DetailFacts]] = Answer.Unknown
    def candidates(q: CandidateQuery): Answer[Seq[Hit]] = if (q == CandidateQuery.Title("Heat")) Answer.Known(Nil) else Answer.Unknown
    def film(id: Int): Answer[Option[IdentityMeasures.Film]] = Answer.Unknown
  }

  "The questions a slice's decision asks" should "be noted when unanswered, as a trace renders them, and only then" in {
    val asking = new IdentitySlice.Asking(recording)
    asking.candidates(CandidateQuery.Title("Heat")) shouldBe Answer.Known(Nil)
    asking.candidates(CandidateQuery.Imdb("When Pigs Fly")) shouldBe Answer.Unknown
    asking.film(149933) shouldBe Answer.Unknown
    asking.unanswered.toArray.toSeq.map(_.toString).sorted shouldBe
      Seq(DecisionTrace.renderQuery(CandidateQuery.Imdb("When Pigs Fly")), "tmdb 149933").sorted
  }

  "A model that moved" should "be the CODE's move when every question was answered" in {
    IdentitySlice.modelMovedWhy(Nil, withLive) should startWith("[code]")
  }

  it should "be a NEW question when the capture keeps every live answer its model read and still lacks it" in {
    IdentitySlice.modelMovedWhy(Seq("""imdb "When Pigs Fly""""), withLive) should startWith("[a: new question]")
  }

  it should "say it cannot tell a new question from an unkept live answer when the capture keeps none" in {
    val why = IdentitySlice.modelMovedWhy(Seq("""imdb "When Pigs Fly"""", "tmdb 149933"), noLive)
    why should startWith("[a or b")
    why should include("""imdb "When Pigs Fly"; tmdb 149933""")
  }
}

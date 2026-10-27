package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity._

/** A capture's recording gives every answer it noted again, never its live source asked anew: its rounds decide over
 *  what the file holds and the replay reads (Identity capture #2079, UK: a capture that replayed to other takes, now
 *  and then). */
class UnmatchedClustersRecordingSpec extends AnyFlatSpec with Matchers {
  /** A live source that answers each question once, then fails it: a host timing out on a later round. */
  private final class AnswersOnce extends IdentityLookups {
    private val asked = scala.collection.mutable.Set.empty[Any]
    private def once[A](question: Any, answer: A): Answer[A] = synchronized {
      if (asked.add(question)) Answer.Known(answer) else Answer.Unknown
    }
    val overviewed: ProseWords = ProseWords.ofProse(Seq("A Digger in the Desert"))
    override def hasDetail(listing: Listing): Boolean                    = false
    override def detail(listing: Listing): Answer[Option[DetailFacts]]   = once(listing.key, None)
    override def candidates(query: CandidateQuery): Answer[Seq[Hit]]     = once(query, Nil)
    override def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = once(("film", tmdbId), None)
    override def cast(tmdbId: Int): Answer[Option[Seq[String]]]          = once(("cast", tmdbId), Some(Seq("Tom Cruise")))
    override def overview(tmdbId: Int): Answer[Option[ProseWords]]       = once(("overview", tmdbId), Some(overviewed))
  }

  "A capture's recording" should "give an answer again when its live source fails the question on a later round" in {
    val live      = new AnswersOnce
    val recording = new UnmatchedClusters.Recording(live)
    val query     = CandidateQuery.Title("Digger")
    Seq.fill(2)((recording.overview(1248832), recording.cast(1248832), recording.film(1248832), recording.candidates(query))) shouldBe
      Seq.fill(2)((Answer.Known(Some(live.overviewed)), Answer.Known(Some(Seq("Tom Cruise"))), Answer.Known(None), Answer.Known(Nil)))
  }

  it should "leave a question its source never answered unknown, and ask it again" in {
    val recording = new UnmatchedClusters.Recording(new IdentityLookups {
      private var asks = 0
      override def hasDetail(listing: Listing): Boolean                    = false
      override def detail(listing: Listing): Answer[Option[DetailFacts]]   = Answer.Unknown
      override def candidates(query: CandidateQuery): Answer[Seq[Hit]]     = Answer.Unknown
      override def film(tmdbId: Int): Answer[Option[IdentityMeasures.Film]] = Answer.Unknown
      override def overview(tmdbId: Int): Answer[Option[ProseWords]]       = { asks += 1; if (asks > 1) Answer.Known(None) else Answer.Unknown }
    })
    Seq(recording.overview(7), recording.overview(7), recording.overview(7)) shouldBe Seq(Answer.Unknown, Answer.Known(None), Answer.Known(None))
  }
}

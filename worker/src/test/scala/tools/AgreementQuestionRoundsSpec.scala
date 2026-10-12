package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{FamilyAnswerStore, FamilySource, InMemoryTmdbDocuments}
import services.identity.agreement.{SourceHit, SourceRecord, VoterFamily}

import java.nio.file.Files
import java.util.concurrent.atomic.AtomicInteger

/** The capture's and the fill's question rounds over a host that never answers one question — the US capture of
 *  2026-10-12 (run 38281127226): Wikidata's search for "SANGGEUN PARK" timed out from the GitHub runner on every one of
 *  twelve rounds, and the capture failed with the question open. */
class AgreementQuestionRoundsSpec extends AnyFlatSpec with Matchers {

  private val Blackholed = VoterFamily.Wiki -> "director|SANGGEUN PARK"
  private val Answering  = VoterFamily.Wiki -> "director|Jineui Kim"

  private final class Source(failFirst: Int = 0) extends FamilySource {
    val blackholedAsks = new AtomicInteger
    val answeringAsks  = new AtomicInteger
    val family: VoterFamily = VoterFamily.Wiki
    def titled(text: String): Seq[SourceHit] = Nil
    def directedBy(name: String): Seq[SourceHit] =
      if (s"director|$name" == Blackholed._2) { blackholedAsks.incrementAndGet(); throw new java.net.SocketTimeoutException("timed out after 30s") }
      else if (answeringAsks.incrementAndGet() <= failFirst) throw new java.net.SocketTimeoutException("timed out after 30s")
      else Seq(SourceHit("Q1", "A film", None, Some(2025)))
    def record(id: String): Option[SourceRecord] = None
  }

  private def rounds(source: Source) = {
    val store = new FamilyAnswerStore(new InMemoryTmdbDocuments, SpecClock.Pinned)
    // the stage's open questions: every one the store holds no answer to
    val open  = () => Set(Blackholed, Answering).filter { case (family, question) => store.wanted(FamilyAnswerStore.questionId(family, question)) }
    store -> new AgreementQuestionRounds(store, Map(VoterFamily.Wiki -> source), "spec").run(open)(identity)
  }

  "the question rounds" should "give a question its host never answers up as unreachable after its attempts, and finish" in {
    val source         = new Source
    val (store, asked) = rounds(source)
    asked.unreachable shouldBe Set(Blackholed)
    asked.open shouldBe empty
    source.blackholedAsks.get shouldBe AgreementQuestionRounds.Attempts
    asked.rounds should be < AgreementQuestionRounds.MaxRounds
    store.wanted(FamilyAnswerStore.questionId(Answering._1, Answering._2)) shouldBe false
  }

  it should "file a question that failed fewer times than its attempts, and give none of it up" in {
    val source         = new Source(failFirst = AgreementQuestionRounds.Attempts - 1)
    val (store, asked) = rounds(source)
    store.wanted(FamilyAnswerStore.questionId(Answering._1, Answering._2)) shouldBe false
    asked.unreachable shouldBe Set(Blackholed)
  }

  "an unreachable question" should "be kept in the fixture beside the answers, for the replay to leave open and the next capture to ask" in {
    val capture = UnmatchedClusters.Capture(models.Country.all.head, Nil, Nil, Map.empty, Map.empty, Map.empty, Set.empty, Map.empty, Map.empty,
      unreachable = Set(Blackholed))
    val file = Files.createTempFile("unmatched", ".json.gz")
    try {
      UnmatchedClusters.write(file, capture)
      val read = UnmatchedClusters.read(file)
      read.unreachable shouldBe Set(Blackholed)
      read.families shouldBe empty
      UnmatchedClusters.unasked(read, Set(Blackholed, Answering)) shouldBe Set(Answering)
    } finally Files.deleteIfExists(file)
  }
}

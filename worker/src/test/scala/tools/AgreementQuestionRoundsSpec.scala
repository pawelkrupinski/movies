package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.adapters.{FamilyAnswerStore, FamilySource, InMemoryTmdbDocuments}
import services.identity.agreement.{SourceHit, SourceRecord, VoterFamily}

import java.nio.file.Files
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.immutable.ListSet

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

  it should "give up every unreachable question of a family, not one per family" in {
    // the PL capture of 2026-10-12 (run 38289527645): ~25 questions of five families timed out on every one of 24 rounds
    // ("attempt 16 of 3") while "5 given up as unreachable" never moved — the given-up set was keyed by family
    val blackholed = Set("Zemsta", "\"Zemsta\"", "DOBRE Kino - Fatima").map(title => VoterFamily.Filmweb -> s"title|$title")
    val asks       = new AtomicInteger
    val source     = new FamilySource {
      val family: VoterFamily = VoterFamily.Filmweb
      def titled(text: String): Seq[SourceHit] = { asks.incrementAndGet(); throw new java.net.SocketTimeoutException("timed out after 30s") }
      def directedBy(name: String): Seq[SourceHit] = Nil
      def record(id: String): Option[SourceRecord] = None
    }
    val store = new FamilyAnswerStore(new InMemoryTmdbDocuments, SpecClock.Pinned)
    val asked = new AgreementQuestionRounds(store, Map(VoterFamily.Filmweb -> source), "spec").run(openOf(store, blackholed))(identity)
    asked.unreachable shouldBe blackholed
    asked.rounds shouldBe AgreementQuestionRounds.Attempts
    asks.get shouldBe blackholed.size * AgreementQuestionRounds.Attempts
  }

  private def openOf(store: FamilyAnswerStore, questions: Set[(VoterFamily, String)]) =
    () => questions.filter { case (family, question) => store.wanted(FamilyAnswerStore.questionId(family, question)) }

  /** A source of `family` answering every ask: one it `counts` counts `release` down, and one it `holds` is held open (a
   *  host waiting out its timeout) until `release` reaches zero — at most [[SpecTimeouts.Settle]], then it fails as a
   *  timeout would, so a regression fails the test rather than hanging it. */
  private final class Held(val family: VoterFamily, release: CountDownLatch, counts: String => Boolean, holds: String => Boolean) extends FamilySource {
    def titled(text: String): Seq[SourceHit] = Nil
    def directedBy(name: String): Seq[SourceHit] = {
      if (counts(name)) release.countDown()
      if (holds(name) && !release.await(SpecTimeouts.Settle.toMillis, TimeUnit.MILLISECONDS))
        throw new java.net.SocketTimeoutException(s"'$name' held past ${SpecTimeouts.Settle}")
      Seq(SourceHit("Q1", "A film", None, Some(2025)))
    }
    def record(id: String): Option[SourceRecord] = None
  }

  it should "ask every family's questions side by side, each family's window full, whatever the machine's cores" in {
    // the PL capture of 2026-10-12 (run 38281127226) on a 4-vCPU runner: ForkJoinPool.commonPool's three threads ran three
    // families at once and one question of each, so every 30s timeout was waited out alone — 7.5 minutes a round
    val perFamily = 4
    val families  = VoterFamily.values.toSeq
    val inFlight  = new CountDownLatch(families.size * perFamily)
    val questions = for { family <- families; n <- 1 to perFamily } yield family -> s"director|Director $n"
    val store     = new FamilyAnswerStore(new InMemoryTmdbDocuments, SpecClock.Pinned)
    val sources   = families.map(f => f -> new Held(f, inFlight, counts = _ => true, holds = _ => true)).toMap
    val asked     = new AgreementQuestionRounds(store, sources, "spec", perFamily = perFamily).run(openOf(store, questions.toSet))(identity)
    asked.unreachable shouldBe empty
    asked.rounds shouldBe 1
  }

  it should "start a family's next question as soon as one of its window is answered, not when the slowest of a batch is" in {
    // one ask held open must not hold back the questions queued behind it
    val quickAnswered = new CountDownLatch(2)
    val slow          = VoterFamily.Wiki -> "director|Held open"
    val quick         = Seq(VoterFamily.Wiki -> "director|First", VoterFamily.Wiki -> "director|Second")
    val store         = new FamilyAnswerStore(new InMemoryTmdbDocuments, SpecClock.Pinned)
    val isSlow        = (name: String) => s"director|$name" == slow._2
    val source        = new Held(VoterFamily.Wiki, quickAnswered, counts = name => !isSlow(name), holds = isSlow)
    val asked         = new AgreementQuestionRounds(store, Map(VoterFamily.Wiki -> source), "spec", perFamily = 2)
      .run(openOf(store, ListSet((slow +: quick)*)))(identity)
    asked.unreachable shouldBe empty
    asked.rounds shouldBe 1
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

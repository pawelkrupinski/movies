package services.identity.agreement

import models.KinoMuza
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{Answer, FilmTable, IdentityCalibration, IdentityMeasures, Listing, Question}
import services.movies.SingleCountryNormalizer

import scala.collection.mutable

/** A family's verdict over a cluster's listings is resolved once and handed back while everything its resolve read reads
 *  alike — noting what it read exactly as a fresh resolve does — and resolved again the moment any of it, an absence
 *  included, or the listings themselves, read otherwise. */
class FamilyVerdictsSpec extends AnyFlatSpec with Matchers {
  import FilmTable.listing

  private val normalizer = SingleCountryNormalizer.titleNormalizer
  private val klondike   = IdentityMeasures.Film("Klondike", None, Nil, Some(2022), Some(100), Some(Seq("Maryna Er Gorbach")), None, None)
  private val held       = new HeldFamilyAnswers(VoterFamily.Imdb, Map("tt16315948" -> SourceRecord(klondike, Map("imdb" -> "tt16315948"))))
  private val bare       = Seq(listing(KinoMuza, "Klondike", year = Some(2022)))

  /** The family's answers as they stand now: what it holds is switched to file an answer. */
  private final class Filing(var now: FamilyAnswers) extends FamilyAnswers {
    val family: VoterFamily = now.family
    def titled(text: String): Answer[Seq[SourceHit]]     = now.titled(text)
    def directedBy(name: String): Answer[Seq[SourceHit]] = now.directedBy(name)
    def record(id: String): Answer[Option[SourceRecord]] = now.record(id)
    override def fresh(question: String): Boolean        = now.fresh(question)
  }

  private def verdicts = new FamilyVerdicts(NoVenueDetails, normalizer, Map(VoterFamily.Imdb -> IdentityCalibration.resolver))

  /** The verdict, its receipt and its gaps, resolved afresh. */
  private def afresh(listings: Seq[Listing], answers: FamilyAnswers) = {
    val (asked, reads) = (mutable.Set.empty[Question], mutable.Map.empty[services.identity.Dependency, Long])
    (Agreement.verdict(listings, new AgreementStage.Recording(answers, asked, reads), NoVenueDetails, normalizer, IdentityCalibration.resolver), asked.toSet, reads.toMap)
  }
  private def through(kept: FamilyVerdicts, listings: Seq[Listing], answers: FamilyAnswers) = {
    val (asked, reads) = (mutable.Set.empty[Question], mutable.Map.empty[services.identity.Dependency, Long])
    (kept.verdict(listings, answers, asked, reads), asked.toSet, reads.toMap)
  }

  "A family's verdict" should "be handed back, read as a fresh resolve reads it, while every answer it read reads alike" in {
    val kept = verdicts
    val fresh = afresh(bare, held)
    fresh._1.answered.flatMap(_.pick).map(_.id) shouldBe Some("tt16315948")
    through(kept, bare, held) shouldBe fresh
    through(kept, bare, held) shouldBe fresh
    (kept.resolved, kept.handedBack) shouldBe ((1L, 1L))
  }

  it should "be resolved again once an answer it read is filed — an answer filed where none was too" in {
    val kept   = verdicts
    val filing = new Filing(new HeldFamilyAnswers(VoterFamily.Imdb, Map.empty, unanswered = true))
    through(kept, bare, filing)._1 shouldBe Answer.Unknown
    filing.now = held
    through(kept, bare, filing) shouldBe afresh(bare, held)
    (kept.resolved, kept.handedBack) shouldBe ((2L, 0L))
  }

  it should "note its gaps again as a fresh resolve would: a stale answer read is asked again" in {
    val kept   = verdicts
    val filing = new Filing(held)
    through(kept, bare, filing)._2 shouldBe empty
    filing.now = new HeldFamilyAnswers(VoterFamily.Imdb, Map("tt16315948" -> SourceRecord(klondike, Map("imdb" -> "tt16315948"))), stale = true)
    val again = through(kept, bare, filing)
    again shouldBe afresh(bare, filing.now)
    again._2 should not be empty
    kept.handedBack shouldBe 1L
  }

  it should "be resolved again over listings that moved, though their keys did not" in {
    val kept  = verdicts
    through(kept, bare, held)
    val moved = bare.map(_.copy(year = Some(1932)))
    through(kept, moved, held) shouldBe afresh(moved, held)
    (kept.resolved, kept.handedBack) shouldBe ((2L, 0L))
  }

  // The input gate reads many listings' questions, each a verdict resolve: the replay reads them side by side
  // (scripts.IdentityDrift's Worker), which one lock held across every resolve made one at a time.
  it should "resolve verdicts over other listings at once, each as a fresh resolve reads it" in {
    val kept    = verdicts
    val other   = Seq(listing(KinoMuza, "Nosferatu", year = Some(1922)))
    // each resolve's first answer read waits for the other's: both inside a resolve at the same moment, or neither returns
    val meeting = new java.util.concurrent.CyclicBarrier(2)
    final class Meeting(inner: FamilyAnswers) extends FamilyAnswers {
      val family: VoterFamily = inner.family
      private val met = ThreadLocal.withInitial[java.lang.Boolean](() => false)
      private def meet(): Unit = if (!met.get) { met.set(true); val _ = meeting.await(10, java.util.concurrent.TimeUnit.SECONDS) }
      def titled(text: String): Answer[Seq[SourceHit]]     = { meet(); inner.titled(text) }
      def directedBy(name: String): Answer[Seq[SourceHit]] = { meet(); inner.directedBy(name) }
      def record(id: String): Answer[Option[SourceRecord]] = { meet(); inner.record(id) }
      override def fresh(question: String): Boolean        = inner.fresh(question)
    }
    val answers = new Meeting(held)
    val pool    = java.util.concurrent.Executors.newFixedThreadPool(2)
    try {
      val both = Seq(bare, other).map(listings => pool.submit(() => through(kept, listings, answers)))
      both.map(_.get(30, java.util.concurrent.TimeUnit.SECONDS)) shouldBe Seq(afresh(bare, held), afresh(other, held))
    } finally { val _ = pool.shutdownNow() }
    kept.resolved shouldBe 2L
  }

  it should "be dropped once no apply used it for a while" in {
    val kept = verdicts
    through(kept, bare, held)
    (0 to FamilyVerdicts.KeptApplies).foreach(_ => kept.applied())
    through(kept, bare, held) shouldBe afresh(bare, held)
    (kept.resolved, kept.handedBack) shouldBe ((2L, 0L))
  }
}

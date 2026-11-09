package services.identity.agreement

import services.identity.{Answer, DetailFacts, ForwardingIdentityLookups, IdentityCalibration, IdentityLookups, Listing}
import services.movies.{ListingKey, TitleNormalizer}

import scala.collection.mutable

/** Each family's verdict over a cluster's listings ([[Agreement.verdict]]), kept with everything its resolve read — each of
 *  the family's answers and each venue page, by the digest of what it read, nothing there yet too — and handed back while
 *  every one of them reads the same again. A take's correction read afresh because one family's answer was filed asked
 *  every other family's resolve again over the same listings: ~40% of a drift replay's CPU (PL, 2026-10-26 profile).
 *
 *  A kept verdict is the one a resolve now would make, never the stage's past standing in for it (identity-no-stickiness):
 *  it is keyed by its listings, compared whole, and handed back only once every answer the resolve read reads alike now —
 *  the resolve is a function of those and of the stage's fixed normaliser and calibrations. Handing it back notes what it
 *  read in the caller's receipt and gaps exactly as reading it afresh does ([[AgreementStage.noted]]).
 *
 *  Kept for [[FamilyVerdicts.KeptApplies]] applies after its last use: a quiet production tick reads no verdict, so what a
 *  burst of applies (a boot, a settle) shares is dropped once it is over. Its own lock guards what is kept alone ([[verdict]]). */
private[agreement] final class FamilyVerdicts(venues: IdentityLookups, normalizer: TitleNormalizer, calibrations: Map[VoterFamily, IdentityCalibration]) {
  import FamilyVerdicts.*

  private val kept    = mutable.HashMap.empty[(VoterFamily, Seq[ListingKey]), Kept]
  private var applies = 0L
  /** How many verdicts were resolved and how many handed back kept, since the stage was made. */
  private[agreement] var resolved, handedBack = 0L

  /** `answers`' family's verdict over `listings`, noting what it read in `asked` and `reads` as a [[AgreementStage.Recording]] does.
   *  A verdict not kept is resolved, and a kept one checked against what it read, OUTSIDE the lock, so verdicts over other
   *  listings are read side by side (the model's input gate reading many listings' questions at once: `scripts.IdentityDrift`'s
   *  replay): the lock guards what is kept, never a resolve nor a check, which read the answers and a kept verdict's own
   *  immutable tape alone. Two callers resolving one verdict at once both keep it, alike: a function of its reads. */
  def verdict(listings: Seq[Listing], answers: FamilyAnswers, asked: mutable.Set[services.identity.Question],
              reads: mutable.Map[services.identity.Dependency, Long]): Answer[FamilyVerdict] = {
    val key    = (answers.family, listings.map(_.key))
    val handed = synchronized(kept.get(key)).filter(k => k.listings == listings && k.read.forall(_.stands(answers, venues)))
    handed match {
      case Some(k) =>
        synchronized {
          k.usedAt = applies
          handedBack += 1
        }
        k.read.foreach(_.note(answers, asked, reads))
        k.verdict
      case None =>
        val tape    = mutable.ArrayBuffer.empty[Read]
        val verdict = Agreement.verdict(listings, new AgreementStage.Recording(new Taping(answers, tape), asked, reads),
          new TapingVenues(venues, tape), normalizer, calibrations(answers.family))
        synchronized {
          kept(key) = Kept(listings, tape.distinct.toArray, verdict, applies)
          resolved += 1
        }
        verdict
    }
  }

  /** An apply ended: the verdicts none of the last [[KeptApplies]] applies used are dropped. */
  def applied(): Unit = synchronized {
    applies += 1
    kept.filterInPlace((_, k) => applies - k.usedAt <= KeptApplies)
  }
}

private[agreement] object FamilyVerdicts {
  /** How many applies a verdict no apply used is kept for. */
  val KeptApplies = 8

  private final case class Kept(listings: Seq[Listing], read: Array[Read], verdict: Answer[FamilyVerdict], var usedAt: Long)

  private def digestOf(answer: Answer[?]): Long = services.identity.Dependency.digest(answer)

  /** One thing a verdict's resolve read, by the digest of what it read. */
  private sealed trait Read {
    def stands(answers: FamilyAnswers, venues: IdentityLookups): Boolean
    /** Noted as a [[AgreementStage.Recording]] notes it: a venue page is in no receipt of a family's verdict. */
    def note(answers: FamilyAnswers, asked: mutable.Set[services.identity.Question], reads: mutable.Map[services.identity.Dependency, Long]): Unit = ()
  }
  /** A family's answer to `question` (`title|…`, `director|…`, `record|…`, `showing|…`, as [[AgreementStage.Recording]] names it). */
  private final case class FamilyRead(question: String, digest: Long, unknown: Boolean) extends Read {
    def stands(answers: FamilyAnswers, venues: IdentityLookups): Boolean = digestOf(ask(answers, question)) == digest
    override def note(answers: FamilyAnswers, asked: mutable.Set[services.identity.Question], reads: mutable.Map[services.identity.Dependency, Long]): Unit =
      AgreementStage.noted(answers, question, digest, unknown, asked, reads)
  }
  private final case class DetailRead(listing: Listing, digest: Long) extends Read {
    def stands(answers: FamilyAnswers, venues: IdentityLookups): Boolean = digestOf(venues.detail(listing)) == digest
  }
  private final case class HasDetailRead(listing: Listing, has: Boolean) extends Read {
    def stands(answers: FamilyAnswers, venues: IdentityLookups): Boolean = venues.hasDetail(listing) == has
  }

  private def ask(answers: FamilyAnswers, question: String): Answer[?] = question.split("\\|", 2) match {
    case Array("title", text)     => answers.titled(text)
    case Array("director", name)  => answers.directedBy(name)
    case Array("record", id)      => answers.record(id)
    case Array("showing", venue)  => answers.showing(venue)
    case _                        => throw new IllegalArgumentException(s"no family question $question")
  }

  /** `inner`, each answer it gives taped. */
  private final class Taping(inner: FamilyAnswers, tape: mutable.ArrayBuffer[Read]) extends FamilyAnswers {
    val family: VoterFamily = inner.family
    private def taped[A](question: String, answer: Answer[A]): Answer[A] = {
      tape += FamilyRead(question, digestOf(answer), answer == Answer.Unknown)
      answer
    }
    def titled(text: String): Answer[Seq[SourceHit]]     = taped(s"title|$text", inner.titled(text))
    def directedBy(name: String): Answer[Seq[SourceHit]] = taped(s"director|$name", inner.directedBy(name))
    def record(id: String): Answer[Option[SourceRecord]] = taped(s"record|$id", inner.record(id))
    override def showing(venue: String): Answer[Seq[Showing]] = taped(s"showing|$venue", inner.showing(venue))
    override def fresh(question: String): Boolean        = inner.fresh(question)
  }

  /** The venues' lookups, each venue page a resolve reads taped. */
  private final class TapingVenues(inner: IdentityLookups, tape: mutable.ArrayBuffer[Read]) extends ForwardingIdentityLookups(inner) {
    override def hasDetail(listing: Listing): Boolean = { val has = inner.hasDetail(listing); tape += HasDetailRead(listing, has); has }
    override def detail(listing: Listing): Answer[Option[DetailFacts]] = { val answer = inner.detail(listing); tape += DetailRead(listing, digestOf(answer)); answer }
  }
}

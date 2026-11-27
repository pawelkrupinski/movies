package services.identity.agreement

import scala.collection.mutable

import models.ListingKey
import services.identity.measures.Listing
import services.identity.vocabulary.{Answer, VoterFamily}
import services.identity.{IdentityCalibration, IdentityLookups, ReadEvent, RecordingLookups}
import services.titles.TitleNormalizer

/** Each family's verdict over a cluster's listings ([[FamilyIdentification.verdict]]), kept with everything its resolve read — each of
 *  the family's answers and each venue page, by the digest of what it read, nothing there yet too — and handed back while
 *  every one of them reads the same again. A take's correction read afresh because one family's answer was filed asked
 *  every other family's resolve again over the same listings: ~40% of a drift replay's CPU (PL, 2026-10-26 profile).
 *
 *  A kept verdict is the one a resolve now would make, never the stage's past standing in for it (identity-no-stickiness):
 *  it is keyed by its listings, compared whole, and handed back only once every answer the resolve read reads alike now —
 *  the resolve is a function of those and of the stage's fixed normaliser and calibrations. Handing it back notes what it
 *  read in the caller's receipt and gaps exactly as reading it afresh does ([[AgreementEvidence.noted]]).
 *
 *  Kept for [[FamilyVerdicts.KeptApplies]] applies after its last use: a quiet production tick reads no verdict, so what a
 *  burst of applies (a boot, a settle) shares is dropped once it is over. Its own lock guards what is kept alone ([[verdict]]). */
private[agreement] final class FamilyVerdicts(venues: IdentityLookups, normalizer: TitleNormalizer, calibrations: Map[VoterFamily, IdentityCalibration]) {
  import FamilyVerdicts.*

  /** Each family's identification of a cluster, under the stage's normaliser and the family's calibration. */
  private val identifications: Map[VoterFamily, FamilyIdentification] =
    calibrations.map { case (family, calibration) => family -> new FamilyIdentification(normalizer, calibration) }
  private val kept    = mutable.HashMap.empty[(VoterFamily, Seq[ListingKey]), Kept]
  private var applies = 0L
  /** How many verdicts were resolved and how many handed back kept, since the stage was made. */
  private[agreement] var resolved, handedBack = 0L

  /** `answers`' family's verdict over `listings`, noting what it read in `asked` and `reads` as a [[AgreementEvidence.recording]] does.
   *  A verdict not kept is resolved, and a kept one checked against what it read, OUTSIDE the lock, so verdicts over other
   *  listings are read side by side (the model's input gate reading many listings' questions at once: `scripts.IdentityDrift`'s
   *  replay): the lock guards what is kept, never a resolve nor a check, which read the answers and a kept verdict's own
   *  immutable tape alone. Two callers resolving one verdict at once both keep it, alike: a function of its reads. */
  def verdict(listings: Seq[Listing], answers: FamilyAnswers, asked: mutable.Set[services.identity.vocabulary.Question],
              reads: mutable.Map[services.identity.vocabulary.Dependency, Long]): Answer[FamilyVerdict] = {
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
        val verdict = identifications(answers.family).verdict(listings, AgreementEvidence.recording(answers, asked, reads, taping(tape)),
          new RecordingLookups(venues, taping(tape)))
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

  private def digestOf(answer: Answer[?]): Long = services.identity.vocabulary.Dependency.digest(answer)

  /** One thing a verdict's resolve read, by the digest of what it read. */
  private sealed trait Read {
    def stands(answers: FamilyAnswers, venues: IdentityLookups): Boolean
    /** Noted as a [[AgreementEvidence.recording]] notes it: a venue page is in no receipt of a family's verdict. */
    def note(answers: FamilyAnswers, asked: mutable.Set[services.identity.vocabulary.Question], reads: mutable.Map[services.identity.vocabulary.Dependency, Long]): Unit = ()
  }
  /** A family's answer to `question` (`title|…`, `director|…`, `record|…`, `showing|…`, as [[AgreementEvidence.recording]] names it). */
  private final case class FamilyRead(question: String, digest: Long, unknown: Boolean) extends Read {
    def stands(answers: FamilyAnswers, venues: IdentityLookups): Boolean = digestOf(ask(answers, question)) == digest
    override def note(answers: FamilyAnswers, asked: mutable.Set[services.identity.vocabulary.Question], reads: mutable.Map[services.identity.vocabulary.Dependency, Long]): Unit =
      AgreementEvidence.noted(answers, question, digest, unknown, asked, reads)
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

  /** What a resolve reads of the venues' pages, taped. */
  private def taping(tape: mutable.ArrayBuffer[Read]): ReadEvent => Unit = {
    case ReadEvent.FamilyAnswered(question, answer)  => tape += FamilyRead(question, digestOf(answer), answer == Answer.Unknown)
    case ReadEvent.DetailAnswered(listing, answer)   => tape += DetailRead(listing, digestOf(answer))
    case ReadEvent.DetailPresence(listing, has)      => tape += HasDetailRead(listing, has)
    case _: ReadEvent.LookupAnswered                 => ()
  }
}

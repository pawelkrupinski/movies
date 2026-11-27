package services.identity.agreement

import scala.collection.mutable

import models.ListingKey
import services.identity.agreement.StageWaits.ClusterWait
import services.identity.measures.{Listing, ResolverDecision}
import services.identity.vocabulary.{Dependency, Question, VoterFamily}

/** The stage's reading of a film the MODEL took against the evidence that can contradict it ([[Correction]]): the
 *  stored correction while it stands, else the relay, the venue posters, the venues' programmes on `listedOn`'s site and
 *  the families read anew ([[CorrectionReading]]). It reads through `evidence` ([[AgreementEvidence]]) and the take's
 *  `candidates`, finds and files its
 *  corrections among the stage's `verdicts` — one decided under other rules than `decidedUnder` read again — asks the
 *  families of a take met in a new shape `inline`.
 *  What a correction still waits on is kept here ([[correctionWaits]]). */
private[agreement] final class AgreementCorrections(evidence: AgreementEvidence, candidates: AgreementCandidates, verdicts: VerdictIndex,
                                                   inline: InlineAsker, decidedUnder: String,
                                                   listedOn: Option[VoterFamily]) {
  import evidence.tried

  /** The evidence against a take, read afresh ([[readAfresh]]). */
  private val reading = new CorrectionReading(evidence, candidates, listedOn)

  /** What a correction still waiting on an answer asks, by its stored id, at the digest of the take's listings it was
   *  read at: a take's posters, families' questions and finds while the backfill hashes them — none kept once its
   *  evidence is all answered, nor once the take's listings move. */
  val correctionWaits: scala.collection.concurrent.TrieMap[String, ClusterWait] = scala.collection.concurrent.TrieMap.empty

  /** The correction held for the take `key` names over listings digesting to `digest` ([[VerdictIndex.heldFor]]): one
   *  found under another key filed under this one, what it waited on not carried. */
  private def heldFor(key: String, digest: Long, pass: AgreementPass): Option[StoredVerdict] =
    verdicts.heldFor(key, digest, { under => pass.moved += under; correctionWaits -= key })

  /** A model take's correction the stage meets in this SHAPE (`digest`, [[correctionDigest]]) for the first time since it
   *  started — none held of that shape under any key, none asked inline — past its first pass: what the stage asks the
   *  families of inline ([[InlineAsker]]), as it does a new cluster's. */
  private def newCorrection(digest: Long): Boolean = inline.mayAsk(digest, verdicts.byListings(digest, correction = true).isDefined)

  /** A film the model took by its own rules — what [[Correction]] reads the evidence against. */
  def correctable(decision: ResolverDecision): Boolean =
    decision.film.isDefined && decision.members.nonEmpty &&
      (decision.basis == ResolverDecision.Basis.OwnMatch || decision.basis == ResolverDecision.Basis.PooledMatch)

  /** The model's take, withdrawn or switched where the evidence against it says so ([[Correction.decide]]): the stored
   *  correction while its listings and rules stand and none of the answers it read was filed again since it was checked
   *  — at boot, every stored one stands, as a stored verdict does — else read anew: at once when no decided correction
   *  stands for it (never read at this shape, or read waiting on an answer), else while the apply's `fresh` budget lasts
   *  ([[AgreementStage.CorrectionsPerApply]]; a take beyond it stands as last decided until a later apply, never as the
   *  model took it). What it waits on is noted in `pass` to ask: the families' questions, the IMDb ids, the posters. */
  def corrected(decision: ResolverDecision, listingOf: ListingKey => Option[Listing], key: String, pass: AgreementPass): ResolverDecision = {
    val digest = StoredCorrection.digestOf(decision, listingOf)
    // what the correction waited on was read at one shape of the take: another's is no wait of this one
    if (correctionWaits.get(key).exists(_.listings != digest)) correctionWaits -= key
    val kept = keptCorrection(decision, listingOf, key, digest, pass)
    // what stands while the budget is spent: a correction decided over the same listings, under any rules. A take with
    // none — never read at this shape, or read waiting on an answer — would be served as the model took it until a
    // later apply's budget reached it, then corrected at an apply where none of its inputs moved: an interim take
    val standing = heldFor(key, digest, pass).flatMap(_.correction).filterNot(_.waiting)
    val now = kept.orElse(Option.when(standing.isEmpty || pass.fresh())(readAfresh(decision, listingOf, key, digest, pass))).orElse(
      // not read afresh this apply — its budget spent, a deploy's new rules having made every stored verdict stale — it
      // stands as last decided over the same listings, never as the model took it: else the boot's whole projection put
      // the model's take back and a later apply corrected it again, a flip per deploy (UK Chichester's "The Ties That Bind
      // Us", 18 of them). Left unchecked, it is read afresh once an apply's budget reaches it.
      standing)
    correctionWaits.get(key).foreach(_.gaps.foreach {
      case Question.Poster(poster) => pass.correctionPosters += poster
      case question                => pass.asked += question
    })
    now.fold(decision)(_.takenAs(decision))
  }

  /** The take's stored correction while it stands: decided under these rules over the same listings (`digest`), not
   *  waiting on what was all attempted since, and none of its reads filed since it was checked ([[Memo.clean]]). */
  private def keptCorrection(decision: ResolverDecision, listingOf: ListingKey => Option[Listing], key: String, digest: Long,
                             pass: AgreementPass): Option[StoredCorrection] =
    heldFor(key, digest, pass).filter(_.rules == decidedUnder).flatMap(v => v.correction.filter { c =>
      val waits = correctionWaits.get(key)
      // one waiting on an answer is read again unless what it asks is known — after a restart it is not — and once all
      // it waits on was attempted (the settle rule: decided on what it has); a TMDB find it waits on is filed among the
      // answers' changes as the families' answers are, and read again below like them
      // stands when none of its reads was filed since it was checked ([[Memo.clean]]) — a TMDB lookup's filing read again
      // and compared, as a verdict's is: every TMDB search is filed under one id (QueryFiled), so any search filed anywhere
      // reached every take that read one, and each was read afresh — 700 PL takes an apply, the budget never caught up (the
      // drift replay timing out, run 1641). A family's answer, a poster or a question asked that was filed decides it again.
      !(c.waiting && (waits.isEmpty || waits.exists(_.gaps.forall(tried)))) && {
        lazy val listings = AgreementEvidence.sortedListings(decision, listingOf)
        verdicts.clean(key, digest, pass.version, again = {
          case (read: Dependency.Lookup, answered) => evidence.digestNow(read, AgreementEvidence.listingIn(listings)).contains(answered)
          case _                                   => false
        })
      }
    })

  /** The take's correction read afresh ([[CorrectionReading.correctionOf]]) — its families asked inline when its shape is new
   *  ([[newCorrection]]) — stored with its receipt and checked at the apply's version, what it waits on kept. */
  private def readAfresh(decision: ResolverDecision, listingOf: ListingKey => Option[Listing], key: String, digest: Long,
                         pass: AgreementPass): StoredCorrection = {
    val version = pass.version
    // the take's listings sorted and digested only to be read afresh: a stored correction is checked by `digest` alone
    val listings = AgreementEvidence.sortedListings(decision, listingOf)
    // its receipt: every TMDB record and search, family answer and poster it read and every question it asked, so a
    // filing of one reads it again ([[Noting]])
    val receipt = mutable.Map.empty[Dependency, Long]
    def read() = { receipt.clear(); evidence.receipting(receipt)(reading.correctionOf(decision.film.get, AgreementEvidence.Cluster(key, listings, digest), pass.reading)) }
    val (correction, waits) = {
      val first  = read()
      val asking = unaskedFamilies(first._2)
      if (asking.isEmpty || !newCorrection(digest)) first
      else {
        // a take of a newly met shape — a listing joined it, a scrape reshaped it: the families its correction waits on
        // asked now, inline, as a new cluster's are, so its first take reads their answers — else it was served as the
        // model took it until they came in, and corrected after (PL "Ktoś całkiem obcy", 2026-10-25: 21 s on 2007's film)
        inline.asked += digest
        pass.shapes += digest
        // and the questions those answers lead to (a family's search's records), a round each, asking only what no
        // round before did, all within the one bound -- as a new cluster's inline ask follows them
        // ([[AgreementStage.InlineRounds]]). One round left them on the queue: UK Chichester's "The Ties That Bind
        // Us" took 2008's film when its record was filed, its correction waiting on the wiki, IMDb, Rotten Tomatoes and
        // Metacritic records the first round's searches named, and was corrected rounds later (drift slice, seed 3)
        inline.askedInline(first, asking)(() => read())((now, asked) => unaskedFamilies(now._2).filterNot(asked))
      }
    }
    val verdict = StoredVerdict(key, digest, receipt.toMap, None, decidedUnder, correction = Some(correction))
    if (!verdicts.get(key).contains(verdict)) { pass.moved += verdict; verdicts.file(verdict) }
    verdicts.check(key, digest, version)
    waits.fold(correctionWaits -= key)(w => correctionWaits(key) = ClusterWait(digest, w))
    correction
  }

  /** The families' questions among what a correction waits on (`waits`) not attempted yet: what its inline ask asks. */
  private def unaskedFamilies(waits: Option[Set[Question]]): Set[Question] =
    waits.fold(Set.empty[Question])(_.filter { case question: Question.Family => !tried(question); case _ => false })
}

package services.identity.agreement

import services.identity.{IdentityLookups, Listing, ResolverDecision}
import services.identity.measures.IdentityMeasures
import services.identity.vocabulary.{Answer, Dependency, Question}

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

/**
 * The families' VERDICT on each cluster the model left unmatched ([[verdictOf]]): the one held ([[VerdictIndex]]) while
 * its listings and everything it read stand — its receipt, [[StoredVerdict.reads]] — else decided again over every
 * family's answers now ([[Agreement.weighed]], and the fill where none names a film: [[ClusterFill.filledOf]]), or,
 * while a family's answer is a gap, the questions the cluster waits on ([[waiting]]). A cluster met in a new shape has
 * its families asked `inline` first; one that has waited on questions all attempted SETTLES on the families that
 * answered ([[settles]], docs/design/identity-questions.md step 4). Each verdict is decided under `decidedUnder`: one
 * decided under other rules is decided again while the apply's backfill budget lasts.
 */
private[agreement] final class ClusterVerdicts(evidence: AgreementEvidence, verdicts: VerdictIndex, clusterFill: ClusterFill,
                                               inline: InlineAsker, decidedUnder: String, clock: java.time.Clock) {
  /** The clusters a family has not answered for yet, at the `version` and listings' digest they were last resolved at,
   *  with the questions they wait on: not resolved again until one of those is answered or their listings move. */
  val waiting = TrieMap.empty[String, ClusterWait]

  /** Forget the waits of every cluster but those `seen`. */
  def forgetAllBut(seen: collection.Set[String]): Unit = waiting --= waiting.keySet.toSet -- seen

  /** The verdict held for the cluster `key` names with listings digesting to `digest` — under its key, else under
   *  whichever key it was filed by (a restart's keys lost or numbered otherwise, a pre-key verdict under its first
   *  listing): filed under `key` from now, the move stored with the apply's. A verdict stands on its listings and receipt,
   *  never on the key it was filed by. */
  private def heldFor(key: String, digest: Long, pass: AgreementPass): Option[StoredVerdict] =
    verdicts.heldFor(key, digest, pass.moved += _)

  /** A cluster the stage meets in this SHAPE (its listings digesting to `digest`) for the first time since it started —
   *  no verdict, wait or inline ask of that shape held, under ANY key: found by the digest, as a verdict is ([[heldFor]]),
   *  never by the key naming the cluster now — past its first pass, which meets every cluster:
   *  what [[askNow]] asks inline. A new key and a known key a scrape reshaped alike: under its kept key, PL "Bez końca" ×7
   *  (2026-10-23, 10-26) gained a listing whose title no family had been asked, waited on the queue, and was published
   *  lost until the answers came in half a minute later — where a cluster new to its key would have read them. */
  private[agreement] def newCluster(digest: Long): Boolean =
    inline.mayAsk(digest, verdicts.byListings(digest, correction = false).isDefined) && !waiting.valuesIterator.exists(_.listings == digest)

  /** The cluster's verdict: the stored one while its listings and everything it read stand (its receipt,
   *  [[StoredVerdict.reads]]), else the resolver's over the answers now — `None` while a family's is a gap. */
  private[agreement] def verdictOf(decision: ResolverDecision, id: String, listings: Seq[Listing], digest: Long,
                        pass: AgreementPass): Option[StoredVerdict] = {
    // a wait is decided for one shape of the cluster: one kept for another is no wait of this one, whatever decides it now
    if (waiting.get(id).exists(_.listings != digest)) waiting -= id
    val found = heldFor(id, digest, pass)
    // settled without some families: their questions asked on every pass — one answered reads the verdict again
    found.foreach(v => pass.asked ++= unreached(v))
    found.filter(_.rules == decidedUnder).filter(stands(_, id, digest, listings, pass))
    // decided under other rules (a deploy's, or before receipts were whole): decided again while the apply's backfill
    // budget lasts, standing as it was until a later apply's reaches it — a deploy's boot resolves no whole corpus. One
    // under these rules whose answers moved spends none of it: decided again now, never standing on a stale receipt.
    // One decided again to a wait is dropped with the apply ([[forgetGone]]): it neither stands again nor spends a
    // later apply's budget again while its cluster waits
    .orElse(found.filter(_.rules != decidedUnder).filter(_ => !backfilled(id, pass)))
    .orElse(waiting.get(id).filter(w => w.listings == digest && !due(w)) match {
      // its questions not all answered yet: a resolve now could reach no verdict — or, its fill read blind, the verdict
      // that read gave, at answers nothing has been filed over since: a resolve now would read the very same
      case Some(w) => pass.asked ++= w.gaps; w.read
      case None    => resolved(decision, id, digest, listings, pass)
    })
  }

  /** Does `kept`, a verdict of cluster `id` decided under these rules, stand at the apply's version? While none of its
   *  reads was filed since it was checked ([[Memo.clean]]), else while each reads as it did: a stale answer among its
   *  reads, read again, asked again — and, at the full validation's turn, read again whole though the index calls it
   *  standing, a drift decided again. */
  private def stands(kept: StoredVerdict, id: String, digest: Long, listings: Seq[Listing], pass: AgreementPass): Boolean = {
    val version = pass.version
    // a filing of what it read is compared by value, that answer alone read again ([[Memo.clean]]'s `again`): one that
    // moved nothing leaves it standing — every TMDB search is filed under one id (QueryFiled), so any search filed
    // anywhere read every verdict that read one whole (DE 2026-10-27: ~4 s, 1.5 GB an apply)
    var reread = false
    val clean  = verdicts.clean(id, digest, version, again = { (read, recorded) =>
      reread = true
      evidence.digestNow(read, AgreementEvidence.listingIn(listings)).contains(recorded)
    })
    // read again because an answer was filed: a stale one among its reads is asked again too — a quiet tick reads none
    def askStale(): Unit = pass.asked ++= kept.reads.keys.flatMap(evidence.staleOf)
    if (clean && reread) askStale()
    if (clean && pass.validating()) {
      // the full validation's: read again whole, though the index calls it standing — a drift is decided again
      val checked = verdicts.memo.verified(id, kept, evidence.digestNow(_, AgreementEvidence.listingIn(listings)))
      pass.validated += checked
      checked.drifted == 0
    } else clean || {
      val stands = verdicts.memo.stands(kept, evidence.digestNow(_, AgreementEvidence.listingIn(listings)))
      if (stands) { verdicts.check(id, digest, version); askStale() }
      stands
    }
  }

  /** Is cluster `id`'s verdict, decided under other rules, decided again this apply — its backfill budget not spent? */
  private def backfilled(id: String, pass: AgreementPass): Boolean = {
    val again = pass.backfill()
    if (again) pass.backfilled += id
    again
  }

  /** The cluster's verdict, the resolver asked again over every family's answers now — `None`, and the cluster kept
   *  waiting on its questions, while one is a gap. */
  private def resolved(decision: ResolverDecision, id: String, digest: Long, listings: Seq[Listing],
                       pass: AgreementPass): Option[StoredVerdict] = {
    pass.resolves += 1
    // the verdict's receipt: everything its decision reads, each by its id with the digest of what it read — nothing
    // there yet too — so a filing of any of it reads the verdict again ([[Noting]])
    val reads = mutable.Map.empty[Dependency, Long]
    val gaps  = mutable.Set.empty[Question]
    val answers = familiesAsked(id, digest, listings, gaps, reads, pass)
    pass.asked ++= gaps
    val unknown = answers.contains(Answer.Unknown)
    // settled once every question it waits on was attempted: the question memory's to say, never the cluster's past
    val settling = unknown && gaps.forall(evidence.tried)
    if (unknown && !settling) { waiting(id) = ClusterWait(digest, gaps.toSet); None }
    else {
      waiting -= id
      Some(weighedVerdict(decision, id, digest, listings, answers, reads, pass))
    }
  }

  /** Every family's verdict on cluster `id`'s listings, each gap noted in `gaps` and each read in `reads` — a cluster met
   *  in a new shape ([[newCluster]]) having its families asked inline first ([[askedInline]]). */
  private def familiesAsked(id: String, digest: Long, listings: Seq[Listing], gaps: mutable.Set[Question],
                            reads: mutable.Map[Dependency, Long], pass: AgreementPass): Seq[Answer[FamilyVerdict]] = {
    val first = evidence.verdictsOf(listings, gaps, reads)
    if (!first.contains(Answer.Unknown) || !newCluster(digest)) first
    else {
      // a cluster with a newly scraped listing: its families asked now, inline, so its first take reads their answers —
      // and the questions those answers lead to (a search's hits' records), a round each, asking only what no round
      // before did, all within the one bound. Else such a question waited on the queue, and the take came at a later
      // apply none of its own inputs moved at, another cluster's ask having filed it (UK Rio's "Rocky Horror",
      // convergence #2473: the record of a hit Metacritic's search found)
      inline.asked += digest
      pass.inline(id) = (clock.millis(), gaps.toSet)
      inline.askedInline(first, gaps.toSet) { () => gaps.clear(); reads.clear(); evidence.verdictsOf(listings, gaps, reads) } { (now, asked) =>
        if (now.contains(Answer.Unknown)) gaps.toSet -- asked else Set.empty
      }
    }
  }

  /** The verdict the families' answers (`answers`) come to — their agreement weighed, and the fill where none names a
   *  film — with its receipt `reads`: kept and checked at the apply's version, or, its fill having met a TMDB question
   *  not answered yet, held as cluster `id`'s wait and kept by none. */
  private def weighedVerdict(decision: ResolverDecision, id: String, digest: Long, listings: Seq[Listing],
                             answers: Seq[Answer[FamilyVerdict]], reads: mutable.Map[Dependency, Long],
                             pass: AgreementPass): StoredVerdict = {
    val version  = pass.version
    val thisYear = evidence.yearNow
    evidence.readNow(Dependency.Year).foreach(reads(Dependency.Year) = _)
    val known    = answers.flatMap(_.answered)
    val stated   = listings.map(evidence.asStated)
    val (weighed, fill) = evidence.receipting(reads) {
      // the vote's record is TMDB's: the venues' lookups answer venue pages alone, every film unknown (the worker's
      // VenueDetailLookups), so read there the vote linked to no family's pick by its facts in production, while a
      // whole resolve handed one lookups for both linked it -- 26 takes prod kept as none on 2026-10-27 (DE
      // "Rental Family" ×9, "Dolly" ×9, PL "Ice Cream Man" ×4, …): each verdict read its vote's record as unknown
      val lean    = voteOf(decision, evidence.looked.getOrElse(evidence.venuesLooked))
      val weighed = Agreement.weighed(listings, known, lean, Some(thisYear), stated)
      // an agreement only review sites make is taken as nothing ([[taken]]): the fill reads the cluster as one none agreed on
      weighed -> (if (weighed.agreed.exists(_.families.exists(_.namesFilms))) Answer.Known(None)
        else clusterFill.filledOf(decision, listings, known, stated, thisYear, pass.asked, reads))
    }
    val agreed   = weighed.agreed
    // settled: what none answered is in the receipt as unanswered ([[Recording]]), so its answer, once filed, reads
    // the verdict again
    val verdict  = StoredVerdict(id, digest, reads.toMap, agreed, decidedUnder, fill.answered.flatten, refused = weighed.refused)
    if (fill == Answer.Unknown) {
      // the fill met a TMDB question not answered yet: this pass reads the verdict, none keeps it — kept, it stood once
      // TMDB had answered (its reads are the families' only), and the fill was never read again (PL "Lalka (Dolly)" ×5).
      // The wait holds what the pass read, handed back while nothing is filed: dropped, the next decision of the same
      // cluster at the same answers read no verdict and took none — a re-resolve of the family elsewhere flipped the
      // take, the next filing flipped it back (UK Cineworld's "RBO Cinema Season 2026-27: La Fanciulla Del West" ×73)
      waiting(id) = ClusterWait(digest, Set.empty, blindAt = Some(version), read = Some(verdict))
      verdicts.memo.uncheck(id)
    } else {
      if (!verdicts.get(id).contains(verdict)) pass.moved += verdict
      verdicts.check(id, digest, version)
    }
    verdict
  }

  /** Is a waiting cluster due a resolve? Once every question it waits on is answered — any gap left, and the families
   *  could reach no verdict, so resolving on each answer as a backlog drains (every cluster gets one, every tick) only
   *  re-ran the same partial resolve: prod PL 2026-10-04, 25–48 s an agreement phase after the per-question fix — or once
   *  it settles ([[settles]]): then it is decided on the families that answered. */
  private def due(waiting: ClusterWait): Boolean =
    // a fill read blind waits on no family's question but on TMDB's answer: due once something is filed, not every pass
    waiting.blindAt.fold(waiting.gaps.forall(evidence.answered))(_ != evidence.version) || settles(waiting)

  /** The one settle rule (docs/design/identity-questions.md, step 4): a cluster every one of whose questions has been
   *  attempted ([[tried]]) is decided on the families that answered, the rest left out as no verdict — no clock, and a
   *  question merely handed to `ask` holds it. Settling ends the wait, not the asking: its unanswered questions stay in
   *  [[unreached]], asked on every pass, and the first of them answered reads the cluster again ([[verdictOf]]). */
  private def settles(waiting: ClusterWait): Boolean =
    waiting.blindAt.isEmpty && waiting.gaps.forall(evidence.tried)

  /** The questions a settled verdict's families never answered ([[settles]]): its receipt's unanswered entries
   *  ([[Dependency.Unanswered]]) — read off the verdict found by its listings, never kept by a key, so a restart's
   *  keys lost or renumbered ask each cluster's own. */
  private[agreement] def unreached(verdict: StoredVerdict): Iterator[Question] =
    verdict.reads.iterator.collect { case (read, Dependency.Unanswered) => read }.flatMap(evidence.staleOf)

  /** TMDB's own vote on a no-match: the film it leans to, else the best-ranked candidate it weighed — its record as
   *  `lookups` reads it (the verdict's receipt), so it links to a family's pick by facts as well as by ids. */
  private def voteOf(decision: ResolverDecision, lookups: IdentityLookups): Option[SourceRecord] =
    decision.leaning.orElse(decision.candidate).map { vote =>
      val ids = SourceRecord.ofLeaning(vote)
      lookups.film(vote.film).answered.flatten.fold(ids)(film => SourceRecord(film,
        ids.crossIds ++ IdentityMeasures.imdbId(film.imdbNumber).map("imdb" -> _)))
    }
}

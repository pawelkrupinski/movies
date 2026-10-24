package services.identity
package adapters

import play.api.Logging

import java.time.Clock

/**
 * Which of the model's answers to ask again because they have aged, at the ages [[QuestionPolicies]] sets, for the
 * lookup fill to ask after its gaps, in this order:
 *
 *  1. its questions: the one staleness TMDB's change lists cannot report, a brand-new film a stored search or director
 *     walk should now return. A question the model has not settled (a family with no film, or one below
 *     [[QuestionPolicies.Settled]]) is due [[QuestionPolicies.UnsettledAge]] after it was last asked, every other one
 *     [[QuestionPolicies.SettledAge]] after — so a new film reaches every stored search within a week, and the searches
 *     still deciding nothing within a day. Oldest first; a question the store does not hold is a gap, the fill's own.
 *  2. the film records and people the model reads that TMDB last gave longer ago than [[QuestionPolicies.ModelRecordAge]],
 *     at most [[QuestionRenewals.PerRound]] a round, oldest first. Records and people are otherwise renewed only when
 *     TMDB's changes name them (`TmdbChangesSweep`), and an edit the feed never showed — older than its 14 days of
 *     history when the sweep was behind longer, or one it read as irrelevant — left the record as it was for good
 *     (docs/design/identity-questions.md, "an age backstop"). The store is scanned at most every
 *     [[QuestionRenewals.ScanEvery]], and only while the model is taken up (`liveKeys`): an answer it no longer reads is
 *     the store sweep's, not asked again. One fetched again is re-stamped by the store, so the next scan does not name
 *     it; one that failed is named again by the next. A round's share is taken off what is pending whether or not the
 *     round's allowance reaches it.
 *
 * `questions`: each family's questions, and whether every decision in it is settled ([[QuestionRenewals.of]]).
 */
final class QuestionRenewals(store: TmdbStore, documents: TmdbDocumentRetention, language: String,
                             questions: () => Seq[(Set[CandidateQuery], Boolean)], liveKeys: () => Option[Set[String]], clock: Clock,
                             perRound: Int = QuestionRenewals.PerRound)
    extends Logging {
  import QuestionRenewals._

  private var pending   = Vector.empty[Question]
  private var scannedAt = Option.empty[Long]

  /** What this round should ask again: the aged questions, then the round's share of aged records and people. A scan of
   *  the records that throws is tried again the next round, the questions still asked. */
  def due(): Seq[Question] = {
    val queries = aged(questions())
    val records =
      try agedRecords()
      catch {
        case scala.util.control.NonFatal(e) =>
          logger.warn("identity lookup fill: no records or people fetched again for their age, this round", e); Nil
      }
    queries.map(Question.Candidate(_)) ++ records
  }

  private def aged(families: Seq[(Set[CandidateQuery], Boolean)]): Seq[CandidateQuery] = {
    val settledOf = families.flatMap { case (qs, settled) => qs.map(_ -> settled) }
      .groupMapReduce(_._1)(_._2)(_ && _)                     // a question of any unsettled family is unsettled
    val ids  = settledOf.keys.map(q => q -> TmdbStore.questionId(language, q)).toMap
    val held = store.get(TmdbKind.Query, ids.values.toSeq)
    val now  = clock.millis()
    settledOf.toSeq.flatMap { case (query, settled) =>
      val age = if (settled) QuestionPolicies.SettledAge else QuestionPolicies.UnsettledAge
      held.get(ids(query)).flatMap(TmdbStore.fetchedAt).orElse(held.get(ids(query)).map(_ => 0L))
        .filter(at => at < now - age.toMillis).map(query -> _)
    }.sortBy { case (query, at) => (at, query.sortKey) }.map(_._1)
  }

  private def agedRecords(): Seq[Question] = synchronized {
    val now = clock.millis()
    if (pending.isEmpty && scannedAt.forall(now - _ >= ScanEvery.toMillis)) {
      pending = scan(now)
      scannedAt = Some(now)
      if (pending.nonEmpty)
        logger.info(s"TMDB age backstop: ${pending.size} records and people the model reads unfetched for ${QuestionPolicies.ModelRecordAge.toDays}d")
    }
    val (taken, left) = pending.splitAt(perRound)
    pending = left
    taken
  }

  private def scan(now: Long): Vector[Question] = liveKeys().fold(Vector.empty[Question]) { live =>
    def aged(kind: TmdbKind)(of: Int => Question) =
      documents.fetchedBefore(kind, now - QuestionPolicies.ModelRecordAge.toMillis).collect {
        case (id, at) if live.contains(TmdbStore.keyOf(kind, id)) && id.toIntOption.isDefined => at -> of(id.toInt)
      }
    (aged(TmdbKind.Film)(Question.FilmRecord(_)) ++ aged(TmdbKind.Person)(Question.Person(_))).sortBy(_._1).map(_._2).toVector
  }
}

object QuestionRenewals {
  /** The most records and people asked again in one round: a first scan's backlog drains over the day's rounds (48 at
   *  the fill's 30 min), not in one burst. */
  val PerRound: Int = 50
  /** How often the store is scanned for records and people that have aged, at most. */
  val ScanEvery: scala.concurrent.duration.FiniteDuration = scala.concurrent.duration.Duration(6, java.util.concurrent.TimeUnit.HOURS)

  /** Each family's questions, and whether every decision in it is settled. */
  def of(families: Iterable[(Set[CandidateQuery], Seq[ResolverDecision])]): Seq[(Set[CandidateQuery], Boolean)] =
    families.map { case (queries, decisions) =>
      queries -> decisions.forall(d => d.film.isDefined && d.confidence >= QuestionPolicies.Settled)
    }.toSeq
}

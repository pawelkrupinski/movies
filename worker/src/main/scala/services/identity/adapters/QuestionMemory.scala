package services.identity
package adapters

import org.bson.{BsonDocument, BsonInt64}

import java.time.Clock

/**
 * The identity system's one memory of what it asked and got no answer to, keyed by the [[Question]]: the model's
 * questions (a candidate query, a film's record) and every agreement question alike. A question its source could not
 * answer is held back for the wait its kind's policy sets ([[QuestionPolicies]], `nonAnswer`), not asked again on every
 * round, drain or hand-over. The model still reads a held-back question as a gap (`Unknown`, never "no film"), the
 * agreement as the open question it is: only the asking backs off. What the raw capture's stored failure did, before
 * TMDB's answers stopped being kept raw, and what the task queue's backoff did for the agreement's questions until step
 * C3.
 *
 * A marker apart from the answers (`identity_question_markers`, `unanswered|…`), never on one: a real answer that
 * arrives later is read as it is, and the gap is gone. A model question's marker is `unanswered|<the TMDB store's
 * question id>` or `unanswered|film|<tmdbId>`, an agreement question's `unanswered|<its id>` ([[Question.id]]). The
 * model's markers were kept in `tmdb_queries` until 2026-10-24: one there is still read until the sweep's grace deletes
 * it, so the move asks nothing again early. A marker past [[QuestionPolicies.MarkerGrace]] is deleted by the store sweep
 * (`TmdbStoreSweep`): long past its wait, it holds nothing back, and a question no one asks any more would otherwise keep
 * its marker for good.
 *
 * Only a DURABLE non-answer ([[unanswered]]: TMDB's 404/410, an answer the store takes nothing from; a poster given up
 * on) waits its kind's long wait. A question whose ask FAILED ([[failed]]: a 5xx, a timeout, a 429 — what
 * `ReadOutcome.classify` calls a failure) learned nothing, and is asked again after its kind's first failure wait,
 * doubling while it keeps failing, up to the longest: one day for every failure held a whole round's questions back for
 * 24 hours after a ten-minute outage. A venue page is the pipeline's to read and a person's record its director search's:
 * neither is remembered here, and both are always due.
 *
 * Read as an answer is ([[TmdbDocuments.answers]]): the worker's document cache keeps a marker, or its absence, until a
 * write of it — the agreement's open questions are checked here on every hand-over to the queue.
 */
final class QuestionMemory(docs: TmdbDocuments, language: String, clock: Clock) {
  import QuestionMemory._

  /** `questions` without those still inside their wait. */
  def due(questions: Set[Question]): Set[Question] = {
    val ids = questions.iterator.flatMap(question => markerOf(question).map(question -> _)).toMap
    if (ids.isEmpty) questions
    else {
      val held = markers(ids.toSeq)
      val now  = clock.millis()
      // A model question's failure marked before TMDB recovered from an outage is due at once: its backoff was the
      // outage's, and TMDB answers again.
      lazy val recovered = markers(Seq(Health -> HealthMarker)).get(HealthMarker).flatMap(doc => BsonFields.int64(doc.get(RecoveredAt)))
      def waiting(question: Question) = ids.get(question).flatMap(held.get).exists { doc =>
        val failure = failureWaitOf(doc)
        TmdbStore.fetchedAt(doc).exists(at =>
          failure.orElse(policyOf(question).unansweredWait.map(_.toMillis)).exists(wait => at > now - wait) &&
            !(failure.isDefined && question.kind.system == QuestionSystem.Model && recovered.exists(at <= _)))
      }
      questions.filterNot(waiting)
    }
  }

  /** The model's `gaps` without those still inside their wait: a day after a durable non-answer, the failure backoff
   *  after a failed read. Its listings' detail pages are the pipeline's, and pass. */
  def due(gaps: AnswersChanged): AnswersChanged = {
    val asked = due(gaps.queries.map(Question.Candidate(_)) ++ gaps.films.map(Question.FilmRecord(_)))
    gaps.copy(queries = gaps.queries.filter(q => asked(Question.Candidate(q))), films = gaps.films.filter(id => asked(Question.FilmRecord(id))))
  }

  /** The questions asked just now that their source durably left unanswered — or, for a kind given up on, given up on:
   *  each held back its kind's long wait. A kind whose empty answer is filed has nothing to remember. */
  def unanswered(questions: Iterable[Question]): Unit = {
    val marked = questions.iterator.filter(policyOf(_).unansweredWait.isDefined).flatMap(markerOf).toSeq.distinct
    if (marked.nonEmpty) docs.put(TmdbKind.Marker, marked.map(_ -> new BsonDocument(TmdbStore.FetchedAt, BsonInt64(clock.millis()))))
  }

  /** The questions whose ask failed just now — asked again on their kind's failure backoff — and how many times in a
   *  row each has failed now (counted since the last durable non-answer). */
  def failed(questions: Iterable[Question]): Map[Question, Int] = {
    val asked = questions.toSeq.distinct.flatMap(question =>
      for { waits <- policyOf(question).failureWaits; marker <- markerOf(question) } yield (question, marker, waits))
    if (asked.isEmpty) Map.empty
    else {
      val held = markers(asked.map { case (question, marker, _) => question -> marker })
      val now  = clock.millis()
      val marked = asked.map { case (question, marker, (first, max)) =>
        // A marker without a failure wait is a durable one (or none): the backoff starts over.
        val previous = held.get(marker).filter(failureWaitOf(_).isDefined)
        val wait     = previous.flatMap(failureWaitOf).fold(first.toMillis)(p => (p * 2).min(max.toMillis))
        val times    = previous.fold(1)(failuresOf(_) + 1)
        (question, marker -> new BsonDocument(TmdbStore.FetchedAt, BsonInt64(now)).append(FailureWait, BsonInt64(wait))
          .append(Failures, BsonInt64(times.toLong)), times)
      }
      docs.put(TmdbKind.Marker, marked.map(_._2))
      marked.map { case (question, _, times) => question -> times }.toMap
    }
  }

  /** How the fill's round of TMDB reads came out, as a whole. A round where every read FAILED is TMDB down, not a
   *  question it cannot answer; the first round after one where reads answered and none failed is its recovery, and
   *  every model question's failure marked before it is due again at once ([[due]]) — otherwise a long outage left each
   *  question up to the longest failure wait behind a TMDB answering again. A round with answers AND failures is neither:
   *  a question that keeps failing while others answer keeps its own backoff. Nor is a round of fewer than
   *  [[MinOutageReads]] reads: one question failing alone is that question's failure, and calling it an outage released
   *  its backoff every other round. A read counts once it was wanted — failed, or deferred behind the host's back-off (an
   *  outage fails ONE read, then the budget defers the rest to that host). */
  def roundEnded(answered: Int, failed: Int, deferred: Int): Unit = {
    lazy val inOutage = markers(Seq(Health -> HealthMarker)).get(HealthMarker).exists(doc => BsonFields.int64(doc.get(OutageSince)).isDefined)
    val now = clock.millis()
    if (failed > 0 && answered == 0 && failed + deferred >= MinOutageReads)
      docs.put(TmdbKind.Marker, Seq(HealthMarker -> new BsonDocument(TmdbStore.FetchedAt, BsonInt64(now)).append(OutageSince, BsonInt64(now))))
    else if (failed == 0 && answered > 0 && inOutage)
      docs.put(TmdbKind.Marker, Seq(HealthMarker -> new BsonDocument(TmdbStore.FetchedAt, BsonInt64(now)).append(RecoveredAt, BsonInt64(now))))
  }

  /** These markers as held: the markers' own collection's, else a model question's one still in `tmdb_queries` from
   *  before the move. */
  private def markers(ids: Seq[(Question, String)]): Map[String, BsonDocument] = {
    val held   = docs.answers(TmdbKind.Marker, ids.map(_._2).distinct)
    val before = ids.collect { case (question, marker) if question.kind.system == QuestionSystem.Model && !held.contains(marker) => marker }
    if (before.isEmpty) held else docs.get(TmdbKind.Query, before.distinct) ++ held
  }

  private def markerOf(question: Question): Option[String] = question match {
    case Question.Candidate(query)                  => Some(s"$Prefix${TmdbStore.questionId(language, query)}")
    case Question.FilmRecord(tmdbId)                => Some(s"${Prefix}film|$tmdbId")
    case Question.Person(_) | Question.VenuePage(_) => None
    case agreement                                  => agreement.id.map(Prefix + _)
  }

  private def policyOf(question: Question): QuestionPolicies.NonAnswer = QuestionPolicies.of(question.kind).nonAnswer
  private def failureWaitOf(doc: BsonDocument): Option[Long]          = BsonFields.int64(doc.get(FailureWait))
  // a failure marked before the count was kept (the model's, until 2026-10-24) counts once
  private def failuresOf(doc: BsonDocument): Int = BsonFields.int64(doc.get(Failures)).fold(1)(_.toInt)
}

object QuestionMemory {
  val Prefix = "unanswered|"
  /** The marker field holding a failed question's current wait, in millis. */
  private val FailureWait = "failureWaitMs"
  /** The marker field holding how many times in a row a question's ask has failed. */
  private val Failures = "failures"
  /** The fewest reads a round must have wanted, all unanswered, to call TMDB down rather than blame the questions it
   *  asked. */
  val MinOutageReads: Int = 3
  /** The one marker holding TMDB's health across rounds: in an outage since, or recovered at. */
  private val HealthMarker = s"${Prefix}tmdb-health"
  // read as a model question's marker is: one kept in tmdb_queries before the move is still read
  private val Health      = Question.FilmRecord(0)
  private val OutageSince = "outageSinceMs"
  private val RecoveredAt = "recoveredAtMs"
}

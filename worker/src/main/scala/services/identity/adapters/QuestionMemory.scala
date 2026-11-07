package services.identity
package adapters

import org.bson.{BsonDocument, BsonInt64, BsonString}

import java.time.{Clock, Instant}

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
 * What it can answer of a question ([[outcome]]): never attempted as far as it knows (`None`: merely queued is not
 * attempted), [[QuestionMemory.Outcome.Answered]] (the agreement's asker records each answer it files),
 * [[QuestionMemory.Outcome.Absent]] (a durable non-answer, an empty answer filed included) or
 * [[QuestionMemory.Outcome.Failed]] (how many times in a row, when it is due again, and the host the failing request
 * went to when the ask knew it) — kept until the marker is swept. Each host's health is a marker of its own
 * ([[roundEnded]], TMDB's under the id it always had): a host's recovery releases the failures marked against it before.
 * The model's asks mark theirs against the host the read went to as the agreement's do (an IMDb suggestion's is IMDb's);
 * a model question's failure with no host recorded — marked before they did, 2026-10-24 — is TMDB's.
 *
 * Read as an answer is ([[TmdbDocuments.answers]]): the worker's document cache keeps a marker, or its absence, until a
 * write of it — the agreement's open questions are checked here on every hand-over to the queue.
 */
final class QuestionMemory(docs: TmdbDocuments, language: String, clock: Clock) {
  import QuestionMemory._

  private val written = new java.util.concurrent.atomic.AtomicLong
  /** How many times this memory has written its markers: moved only after a write landed (or failed, maybe partly), so a
   *  reader that saw it unmoved since it last read [[attempted]] knows no question has been attempted since — what the
   *  agreement stage's quiet apply checks instead of reading every question it waits on (`AgreementStage.attemptsMarked`).
   *  Sound as the document cache is: every marker write goes through this worker's one memory. */
  def marks: Long = written.get

  private def write(markers: Seq[(String, BsonDocument)]): Unit =
    try docs.put(TmdbKind.Marker, markers) finally { written.incrementAndGet(); () }

  /** `questions` without those still inside their wait. */
  def due(questions: Set[Question]): Set[Question] = {
    val ids = questions.iterator.flatMap(question => markerOf(question).map(question -> _)).toMap
    if (ids.isEmpty) questions
    else {
      val held = markers(ids.toSeq)
      val now  = clock.millis()
      // A failure marked before its host recovered from an outage is due at once: its backoff was the outage's, and the
      // host answers again.
      val recovered = recoveries(ids.toSeq.flatMap { case (question, marker) =>
        held.get(marker).filter(failureWaitOf(_).isDefined).flatMap(hostOf(question, _)) }.toSet)
      def waiting(question: Question) = ids.get(question).flatMap(held.get).filterNot(isAnswered).exists { doc =>
        val failure = failureWaitOf(doc)
        TmdbStore.fetchedAt(doc).exists(at =>
          failure.orElse(policyOf(question).unansweredWait.map(_.toMillis)).exists(wait => at > now - wait) &&
            !(failure.isDefined && hostOf(question, doc).flatMap(recovered.get).exists(at <= _)))
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

  /** The questions asked just now that their source durably left unanswered — an empty answer filed, or for a kind
   *  given up on, given up on: each held back its kind's long wait, none for a kind whose empty answer is filed. */
  def unanswered(questions: Iterable[Question]): Unit = mark(questions, None)

  /** The questions asked just now and answered: attempted, and held back by nothing. */
  def answered(questions: Iterable[Question]): Unit = mark(questions, Some(AnsweredOutcome))

  private def mark(questions: Iterable[Question], outcome: Option[String]): Unit = {
    val marked = questions.iterator.flatMap(markerOf).toSeq.distinct
    if (marked.nonEmpty) write(marked.map { marker =>
      val doc = new BsonDocument(TmdbStore.FetchedAt, BsonInt64(clock.millis()))
      outcome.foreach(o => doc.append(OutcomeField, BsonString(o)))
      marker -> doc
    })
  }

  /** What the memory knows of `question`'s last ask: `None` when it holds none (never attempted, or long enough ago that
   *  its marker was swept). */
  def outcome(question: Question): Option[QuestionMemory.Outcome] =
    markerOf(question).flatMap(marker => markers(Seq(question -> marker)).get(marker)).map(outcomeOf)

  /** What the memory knows of each of `questions`' last ask, read at once: those it holds none of left out. */
  def outcomes(questions: Set[Question]): Map[Question, QuestionMemory.Outcome] = {
    val ids = questions.iterator.flatMap(question => markerOf(question).map(question -> _)).toSeq
    if (ids.isEmpty) Map.empty
    else {
      val held = markers(ids)
      ids.flatMap { case (question, marker) => held.get(marker).map(doc => question -> outcomeOf(doc)) }.toMap
    }
  }

  private def outcomeOf(doc: BsonDocument): QuestionMemory.Outcome = failureWaitOf(doc) match {
    case Some(wait) =>
      Outcome.Failed(failuresOf(doc), Instant.ofEpochMilli(TmdbStore.fetchedAt(doc).getOrElse(0L) + wait), stringOf(doc.get(HostField)))
    case None if isAnswered(doc) => Outcome.Answered
    case None                    => Outcome.Absent
  }

  /** Has `question` been asked and come to an answer, a durable non-answer or a failure — not merely queued? */
  def attempted(question: Question): Boolean = outcome(question).isDefined

  /** Given up on, as the settle rule reads it (`AgreementStage.givenUp`): anything but a failure still inside its
   *  retries -- failed fewer than its kind's [[QuestionPolicies.giveUpFailures]] times in a row, asked again on its
   *  backoff -- which leaves a take settled past it Pending. Answered, durably unanswered, given up on, or held nothing of
   *  (never attempted: the stage waits on that one as unattempted, not by this). */
  def givenUp(question: Question): Boolean = outcome(question) match {
    case Some(failed: Outcome.Failed) => pastGiveUp(question, failed)
    case _                            => true
  }

  /** Failed its kind's give-up count in a row ([[QuestionPolicies.giveUpFailures]]): what the model's stored-first lookups
   *  read as [[Answer.Failed]] (fix 4b) -- a durable non-answer is the store's to file, never read so. */
  def failedPastGiveUp(question: Question): Boolean = outcome(question) match {
    case Some(failed: Outcome.Failed) => pastGiveUp(question, failed)
    case _                            => false
  }

  private def pastGiveUp(question: Question, failed: Outcome.Failed): Boolean =
    failed.attempts >= QuestionPolicies.giveUpFailures(question.kind)

  /** The questions whose ask failed just now — asked again on their kind's failure backoff — and how many times in a
   *  row each has failed now (counted since the last answer or durable non-answer). `host`: where the failing request
   *  went, when the asker knows it. */
  def failed(questions: Iterable[Question], host: Option[String] = None): Map[Question, Int] =
    failing(questions, host)((_, previous, _) => (previous.fold(1)(failuresOf(_) + 1), None))

  /** The questions whose ask was TURNED AWAY just now -- the host's breaker open, or the fleet's pace refusing it
   *  (`CircuitOpenException`): a WAIT, not a failure. Held back on the failure backoff (and released at once when the
   *  host is back, [[recovered]]), but no failure counted: a flapping host turning a poster away eight times gave it up
   *  for good, and with it the poster's evidence. Yet every ask still terminates (fix 1's input gate waits on it): one
   *  turned away for [[QuestionPolicies.TurnAwayGiveUp]] with no real attempt between -- an answer, a non-answer or a
   *  failure ends the run -- counts its kind's give-up failures ([[QuestionPolicies.giveUpFailures]]), a terminal
   *  failure as [[givenUp]] and [[failedPastGiveUp]] read it. The run's start is kept on the marker, so a restart ends no run early or late.
   *  Returns how many failures each counts now. */
  def turnedAway(questions: Iterable[Question], host: Option[String] = None): Map[Question, Int] =
    failing(questions, host) { (question, previous, now) =>
      val since   = previous.flatMap(doc => BsonFields.int64(doc.get(TurnedAwaySince))).getOrElse(now)
      val counted = previous.fold(0)(failuresOf)
      val times   = if (now - since >= QuestionPolicies.TurnAwayGiveUp.toMillis) counted.max(QuestionPolicies.giveUpFailures(question.kind)) else counted
      (times, Some(since))
    }

  /** Marks `questions` failing on their kind's backoff (doubled from the last failure's wait), each counting the failures
   *  `count` says of it -- given its last failure's marker, if any, and now -- and, for a turn-away, when its run of
   *  turn-aways began. */
  private def failing(questions: Iterable[Question], host: Option[String])
                     (count: (Question, Option[BsonDocument], Long) => (Int, Option[Long])): Map[Question, Int] = {
    val asked = questions.toSeq.distinct.flatMap(question =>
      for { waits <- policyOf(question).failureWaits; marker <- markerOf(question) } yield (question, marker, waits))
    if (asked.isEmpty) Map.empty
    else {
      val held = markers(asked.map { case (question, marker, _) => question -> marker })
      val now  = clock.millis()
      val marked = asked.map { case (question, marker, (first, max)) =>
        // A marker without a failure wait is a durable one (or none): the backoff starts over.
        val previous         = held.get(marker).filter(failureWaitOf(_).isDefined)
        val wait             = previous.flatMap(failureWaitOf).fold(first.toMillis)(p => (p * 2).min(max.toMillis))
        val (times, turned)  = count(question, previous, now)
        val doc = new BsonDocument(TmdbStore.FetchedAt, BsonInt64(now)).append(FailureWait, BsonInt64(wait)).append(Failures, BsonInt64(times.toLong))
        turned.foreach(since => doc.append(TurnedAwaySince, BsonInt64(since)))
        host.foreach(h => doc.append(HostField, BsonString(h)))
        (question, marker -> doc, times)
      }
      write(marked.map(_._2))
      marked.map { case (question, _, times) => question -> times }.toMap
    }
  }

  /** How a round of reads of `host` (TMDB's by default; the fill reports each host's reads apart) came out, as a whole.
   *  A round where every read FAILED is the host down, not a
   *  question it cannot answer; the first round after one where reads answered and none failed is its recovery, and
   *  every failure marked against the host before it is due again at once ([[due]]) — otherwise a long outage left each
   *  question up to the longest failure wait behind a host answering again. A round with answers AND failures is neither:
   *  a question that keeps failing while others answer keeps its own backoff. Nor is a round of fewer than
   *  [[MinOutageReads]] reads: one question failing alone is that question's failure, and calling it an outage released
   *  its backoff every other round. A read counts once it was wanted — failed, or deferred behind the host's back-off (an
   *  outage fails ONE read, then the budget defers the rest to that host). */
  def roundEnded(answered: Int, failed: Int, deferred: Int, host: String = TmdbHost): Unit = {
    val health    = healthMarker(host)
    lazy val inOutage = markers(Seq(Health -> health)).get(health).exists(doc => BsonFields.int64(doc.get(OutageSince)).isDefined)
    val now = clock.millis()
    if (failed > 0 && answered == 0 && failed + deferred >= MinOutageReads)
      write(Seq(health -> new BsonDocument(TmdbStore.FetchedAt, BsonInt64(now)).append(OutageSince, BsonInt64(now))))
    else if (failed == 0 && answered > 0 && inOutage)
      write(Seq(health -> new BsonDocument(TmdbStore.FetchedAt, BsonInt64(now)).append(RecoveredAt, BsonInt64(now))))
  }

  /** `host` is back — its breaker closed after an outage, or it answered after a run of failures
   *  ([[HostRecovery]]): every failure marked against it before now is due again at once ([[due]]), as after a round's
   *  recovery ([[roundEnded]]). */
  def recovered(host: String): Unit = {
    val now = clock.millis()
    write(Seq(healthMarker(host) -> new BsonDocument(TmdbStore.FetchedAt, BsonInt64(now)).append(RecoveredAt, BsonInt64(now))))
  }

  /** Those of `questions` whose last ask failed against `host` (a model question's with no host recorded: TMDB's). */
  def failedAgainst(host: String, questions: Set[Question]): Set[Question] = {
    val ids  = questions.iterator.flatMap(question => markerOf(question).map(question -> _)).toSeq
    val held = if (ids.isEmpty) Map.empty[String, BsonDocument] else markers(ids)
    ids.collect { case (question, marker) if held.get(marker).exists(doc => failureWaitOf(doc).isDefined && hostOf(question, doc).contains(host)) =>
      question }.toSet
  }

  /** When each of `hosts` last recovered from an outage, of those that did. */
  private def recoveries(hosts: Set[String]): Map[String, Long] = {
    val ids  = hosts.map(host => host -> healthMarker(host)).toMap
    val held = if (ids.isEmpty) Map.empty[String, BsonDocument] else markers(ids.values.toSeq.map(Health -> _))
    ids.flatMap { case (host, marker) => held.get(marker).flatMap(doc => BsonFields.int64(doc.get(RecoveredAt))).map(host -> _) }
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
  private def isAnswered(doc: BsonDocument): Boolean                  = stringOf(doc.get(OutcomeField)).contains(AnsweredOutcome)
  // the host a failure was marked against: a model question's with none recorded (marked before 2026-10-24) is TMDB's
  private def hostOf(question: Question, doc: BsonDocument): Option[String] =
    stringOf(doc.get(HostField)).orElse(Option.when(question.kind.system == QuestionSystem.Model)(TmdbHost))
  private def stringOf(value: org.bson.BsonValue): Option[String] = Option(value).filter(_.isString).map(_.asString.getValue)
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
  /** The marker field holding when a turned-away question's run of turn-aways began ([[QuestionMemory.turnedAway]]). */
  private val TurnedAwaySince = "turnedAwaySinceMs"
  /** The marker field holding the host a failed question's request went to. */
  private val HostField = "host"
  /** The marker field naming an answered question's outcome; a marker with neither it nor a failure wait is a durable
   *  non-answer. */
  private val OutcomeField    = "outcome"
  private val AnsweredOutcome = "answered"
  /** The host the model's questions are asked of, and whose health marker keeps the id it had before hosts were told
   *  apart. */
  val TmdbHost = "api.themoviedb.org"
  /** The marker holding a host's health across rounds: in an outage since, or recovered at. */
  def healthMarker(host: String): String = if (host == TmdbHost) s"${Prefix}tmdb-health" else s"${Prefix}health|$host"

  /** What the memory knows of a question's last ask. */
  enum Outcome {
    case Answered
    /** Durably unanswered: an empty answer filed, a 404, a poster given up on. */
    case Absent
    /** Failed `attempts` times in a row, due again at `nextRetryAt`; `host` the failing request went to, when known. */
    case Failed(attempts: Int, nextRetryAt: Instant, host: Option[String])
  }
  // read as a model question's marker is: one kept in tmdb_queries before the move is still read
  private val Health      = Question.FilmRecord(0)
  private val OutageSince = "outageSinceMs"
  private val RecoveredAt = "recoveredAtMs"
}

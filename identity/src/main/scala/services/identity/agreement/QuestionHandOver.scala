package services.identity.agreement

import services.identity.vocabulary.Question

/**
 * The agreement stage's questions on their way to the queue: what the last apply met unanswered ([[wanted]]), each
 * handed to `ask` once per families' `version` ([[handOver]]), and what of it was not attempted yet ([[awaiting]]) —
 * one attempted since, though nothing was filed, is what reads the same decisions at the same version again
 * ([[attemptedSince]]). `attemptsMarked`, when given, counts the writes of the question memory: while it marked
 * nothing since, none of them can have been attempted, and a quiet apply reads none of their records.
 *
 * Concurrency: [[handOver]] and [[attemptedSince]] run on the applying thread (the stage's apply is synchronized), which
 * owns everything here but [[wanted]]: a `@volatile` set read from any thread, replaced whole by each [[handOver]].
 */
final class QuestionHandOver(ask: Set[Question] => Unit, attemptsMarked: Option[() => Long]) {
  @volatile private var wantedNow = Set.empty[Question]
  /** The questions handed to `ask` since the families' answers last moved (at `handedAt`): a tick that filed nothing
   *  hands nothing. */
  private var handed   = Set.empty[Question]
  private var handedAt = -1L
  /** The questions the last apply waited on that were not attempted yet. */
  private var awaitingNow = Set.empty[Question]
  /** `attemptsMarked` as it stood when [[awaiting]] was last found unattempted (3,459 records a quiet apply would read
   *  on the e2e corpus, 4.4 MB a tick). */
  private var awaitingMarks: Option[Long] = None

  /** Every question the last apply met unanswered or stale, one each. */
  def wanted: Set[Question] = wantedNow
  /** What of [[wanted]] was not attempted when it was handed over. */
  def awaiting: Set[Question] = awaitingNow

  /** `wanted`, what the apply at `version` met, is what the stage wants now: `ask` is handed what of it was not handed
   *  at `version` yet, and what of it is not `tried` is awaited. */
  def handOver(wanted: Set[Question], version: Long, tried: Question => Boolean): Unit = {
    wantedNow = wanted
    if (handedAt != version) { handed = Set.empty; handedAt = version }
    val open = wanted -- handed
    if (open.nonEmpty) { ask(open); handed = handed ++ open }
    // read before the questions are: a mark landing while they are read moves it, and the next apply reads them again
    awaitingMarks = attemptsMarked.map(_())
    awaitingNow = wanted.filterNot(tried)
  }

  /** Was a question the last apply waited on `tried` since, though nothing was filed — a cluster's wait settling, a
   *  take's evidence read as none? */
  def attemptedSince(tried: Question => Boolean): Boolean = awaitingNow.nonEmpty && {
    val marks = attemptsMarked.map(_())
    (marks.isEmpty || marks != awaitingMarks) && {
      val since = awaitingNow.exists(tried)
      if (!since) awaitingMarks = marks   // marked since, but none of these: not read again until the next mark
      since
    }
  }
}

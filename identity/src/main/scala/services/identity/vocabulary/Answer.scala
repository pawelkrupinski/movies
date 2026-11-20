package services.identity.vocabulary

/** A lookup's answer, one of three and nothing else (docs/design/identity-structural-fixes.md, fix 4):
 *  - `Known`: what the source said, an empty answer (no film, no hit) included;
 *  - `Failed`: TERMINAL -- given up on (`QuestionMemory.failedPastGiveUp`: failed its kind's give-up count in a row); a decision
 *    may read it as no evidence, and is decided on;
 *  - `Unknown`: NOT YET answered -- never asked, in flight, or failed and due again. Never "no film": a decision that
 *    reads one is not decided, it awaits the question ([[ResolverDecision.awaits]]).
 *  The case keeps its name `Unknown` because [[Dependency.digest]] digests an answer by its text: every stored receipt's
 *  unanswered read is `digest(Unknown)` ([[Dependency.Unanswered]]). */
enum Answer[+A] {
  case Known(value: A)
  case Failed
  case Unknown

  /** The answer given, else none -- a failure for good and a question not answered yet alike. A reader that DECIDES on
   *  it reads both as no evidence: for `Failed` that is the rule ([[failedAs]]), for `Unknown` only where the decision is
   *  typed as awaiting it ([[ResolverDecision.awaits]], the stage's waits); every such reader is listed in
   *  docs/design/identity-structural-fixes.md, fix 4d. (Was `toOption`, which named no such duty.) */
  def answered: Option[A] = this match {
    case Known(v)         => Some(v)
    case Failed | Unknown => None
  }
  /** Answered: neither failed nor still awaited. (Was `isKnown`.) */
  def isAnswered: Boolean = this match {
    case Known(_)         => true
    case Failed | Unknown => false
  }
  /** Not answered yet: what a decision awaits ([[ResolverDecision.awaits]]); a failure for good is not. */
  def isAwaited: Boolean = this == Unknown
  def map[B](f: A => B): Answer[B] = this match {
    case Known(v) => Known(f(v))
    case Failed   => Failed
    case Unknown  => Unknown
  }
  def flatMap[B](f: A => Answer[B]): Answer[B] = this match {
    case Known(v) => f(v)
    case Failed   => Failed
    case Unknown  => Unknown
  }
  /** A terminal failure read as `absent`, no evidence -- the ONE way a decision reads a [[Failed]] (fix 4); an answer
   *  not yet given stays Unknown, never "none". */
  def failedAs[B >: A](absent: => B): Answer[B] = this match {
    case Failed => Known(absent)
    case other  => other
  }
  /** This answer, or -- the source not answering it -- `other`'s. */
  def orElse[B >: A](other: => Answer[B]): Answer[B] = if (isAnswered) this else other
}

object Answer {
  /** Every answer's value; else Unknown when any one is still awaited, else Failed. */
  def sequence[A](answers: Seq[Answer[A]]): Answer[Seq[A]] =
    if (answers.forall(_.isAnswered)) Known(answers.flatMap(_.answered))
    else if (answers.contains(Unknown)) Unknown
    else Failed
}

/** What a store holds, as an answer: none held is Unknown. */
extension [A](held: Option[A]) def toAnswer: Answer[A] = held.fold[Answer[A]](Answer.Unknown)(Answer.Known(_))

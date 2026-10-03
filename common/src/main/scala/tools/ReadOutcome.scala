package tools

import scala.util.control.NonFatal

/**
 * What reading an upstream said — the one shape every client reads through, so that
 * **an absent answer is data and a failed read is not**.
 *
 * The bug this exists to make unwritable recurred in OMDb, IMDb, Filmweb, Rotten
 * Tomatoes, the Facebook re-scrape, biletyna, iKsoris, Helios and more: a read that
 * failed — a 403 block, a 5xx, a timeout, or a 200 whose body was a CDN challenge page
 * or an error document — came back as `None` / `Nil` / `"[]"`, byte-for-byte the answer a
 * film nobody rated or a venue with no screenings gives. The caller then acted on the
 * empty value: booked a dead source as a healthy refresh, or landed an empty listing.
 *
 * Three cases, and only two of them are answers:
 *
 *  - [[ReadOutcome.Answered]] — the upstream answered with content that passed its check.
 *  - [[ReadOutcome.Absent]] — the upstream answered "there is nothing here": an explicit
 *    404/410 ([[ReadOutcome.classify]]), or the parser itself recognising a validated empty
 *    answer (an API's `[]` meaning "no results"). Never inferred from a failure.
 *  - [[ReadOutcome.Failed]] — we learned nothing. Every other status, every transport error,
 *    and every 2xx whose body failed its content check ([[ReadFailure.UnexpectedBody]]).
 *
 * Each case explains itself ([[explain]]) so the log line says WHY a read came back the
 * way it did.
 */
sealed trait ReadOutcome[+A] {
  import ReadOutcome._

  def map[B](f: A => B): ReadOutcome[B] = this match {
    case Answered(value)  => Answered(f(value))
    case other: Absent    => other
    case other: Failed    => other
  }

  def flatMap[B](f: A => ReadOutcome[B]): ReadOutcome[B] = this match {
    case Answered(value)  => f(value)
    case other: Absent    => other
    case other: Failed    => other
  }

  /** The bridge to an `Option`-returning client: an answer is `Some`, an absence is `None`,
   *  and a failure is THROWN — so the caller's own retry, backoff and attempt recording
   *  see it, instead of a confident `None`. */
  def toOptionOrThrow: Option[A] = this match {
    case Answered(value)  => Some(value)
    case Absent(_)        => None
    case Failed(cause)    => throw cause.exception
  }

  /** Whether the read taught us nothing — not an answer, not an absence. */
  def isFailed: Boolean = this.isInstanceOf[Failed]

  /** The answer, or `None` when there is none to act on — absent or failed alike. For a caller
   *  whose only safe move without an answer is to do nothing (skip a prune, seed nothing); one that
   *  must tell the two apart matches the cases. */
  def answered: Option[A] = this match {
    case Answered(value) => Some(value)
    case _               => None
  }

  /** For a read whose content is required — a cinema's listing page: anything but an
   *  answer throws. An absence rethrows its original status, so a durable 404 still
   *  reaches the scrape archive as `HTTP 404` (see `GoneUpstream`). */
  def required: A = this match {
    case Answered(value)  => value
    case Absent(reason)   => throw reason.exception
    case Failed(cause)    => throw cause.exception
  }

  /** One line for a log: what came back and why. */
  def explain: String = this match {
    case Answered(_)      => "answered"
    case Absent(reason)   => s"absent: ${reason.explain}"
    case Failed(cause)    => s"failed: ${cause.explain}"
  }
}

object ReadOutcome {

  final case class Answered[+A](value: A) extends ReadOutcome[A]
  final case class Absent(reason: AbsentReason) extends ReadOutcome[Nothing]
  final case class Failed(cause: ReadFailure) extends ReadOutcome[Nothing]

  /** A parser's validated "the upstream says there is none" — the only way besides an
   *  explicit status to reach [[Absent]]. */
  def none(what: String): ReadOutcome[Nothing] = Absent(AbsentReason.EmptyAnswer(what))

  /** A parser's verdict that a 2xx body is not the content the endpoint serves — an
   *  error document of the right syntax, say. Never an absence. */
  def unexpectedBody(url: String, why: String, body: String): ReadOutcome[Nothing] =
    Failed(ReadFailure.UnexpectedBody(new UnexpectedBodyException(url, why, body)))

  /** THE status classifier: a failure is an absence only when it is a typed
   *  [[HttpStatusException]] whose code describes the URL rather than the moment
   *  ([[HttpStatusException.isDurable]] — 404/410). Everything else — a block, a throttle,
   *  a 5xx, a timeout, a reset, a bug — is a failed read. Keyed on the TYPE: a message that
   *  merely reads "HTTP 404" is not an answer, so no wrapper can fake one by accident. */
  def classify(failure: Throwable): Either[AbsentReason, ReadFailure] = failure match {
    case status: HttpStatusException if HttpStatusException.isDurable(status.code) =>
      Left(AbsentReason.NotFound(status))
    case unexpected: UnexpectedBodyException => Right(ReadFailure.UnexpectedBody(unexpected))
    case other                               => Right(ReadFailure.Thrown(other))
  }

  /** Whether a failure is the upstream saying "there is nothing here". */
  def isAbsent(failure: Throwable): Boolean = classify(failure).isLeft

  /** Run `read`, classifying a non-fatal failure. By-name so the read happens inside. */
  def of[A](read: => A): ReadOutcome[A] =
    try Answered(read)
    catch { case NonFatal(failure) => fromFailure(failure) }

  def fromFailure(failure: Throwable): ReadOutcome[Nothing] =
    classify(failure).fold(Absent(_), Failed(_))
}

/** Why a read answered "nothing here". */
sealed trait AbsentReason {
  def explain: String
  /** What a caller that REQUIRES content throws for this absence. */
  def exception: Throwable
}

object AbsentReason {
  /** The upstream's own 404/410. Keeps the typed status so a rethrow is the original. */
  final case class NotFound(status: HttpStatusException) extends AbsentReason {
    def code: Int        = status.code
    def explain: String  = status.getMessage
    def exception: Throwable = status
  }

  /** The parser recognised a validated empty answer (`[]` from a search API). */
  final case class EmptyAnswer(what: String) extends AbsentReason {
    def explain: String  = s"upstream answered none: $what"
    def exception: Throwable = new NoSuchElementException(explain)
  }
}

/** Why a read taught us nothing. */
sealed trait ReadFailure {
  def explain: String
  def exception: Throwable
}

object ReadFailure {
  /** The fetch (or the parse) threw: a non-durable status, a timeout, a reset, a bug. */
  final case class Thrown(exception: Throwable) extends ReadFailure {
    def explain: String = s"${exception.getClass.getSimpleName}: ${exception.getMessage}"
  }

  /** A 2xx whose body is not what the endpoint serves: a challenge page, HTML where JSON
   *  was expected, JSON of the wrong shape, a page missing its expected marker. */
  final case class UnexpectedBody(exception: UnexpectedBodyException) extends ReadFailure {
    def explain: String = exception.getMessage
  }
}

/** A 2xx body that failed its content check. Thrown (via [[ReadOutcome.required]] /
 *  [[ReadOutcome.toOptionOrThrow]]) so it reaches retry, attempt recording and /uptime as
 *  the failure it is. The url is masked in the message ([[RedactedUrl]]) because the
 *  message is what gets logged. */
class UnexpectedBodyException(val url: String, val why: String, excerpt: String)
    extends RuntimeException(
      s"unexpected body from ${RedactedUrl(url)}: $why — starts '${UnexpectedBodyException.excerptOf(excerpt)}'")

object UnexpectedBodyException {
  private val ExcerptLength = 120
  private[tools] def excerptOf(body: String): String =
    body.take(ExcerptLength).replaceAll("\\s+", " ").trim
}

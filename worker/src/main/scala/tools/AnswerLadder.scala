package tools

import scala.util.control.NonFatal

/**
 * A fallback ladder of independent sources asked in order, where the first answer wins —
 * and where "nobody answered" is only a conclusion if everybody was actually asked.
 *
 * A ladder used to protect its later rungs from an earlier one's outage by wrapping each
 * rung in `Try(...).toOption`. That keeps the ladder going, which is right, but it also
 * turns an outage into "no match" when no rung answers, which is wrong: with IMDb's
 * suggestion endpoint blocked and every backstop abstaining, the resolver logged "no match"
 * and moved on, as if IMDb had been asked and had nothing.
 *
 * Here a rung that throws is remembered and the ladder carries on. The first `Some` wins
 * regardless of earlier failures (a later source answered; the outage cost nothing). If no
 * rung answers and any rung FAILED, the ladder throws the first failure (the rest attached
 * as suppressed) — the caller's retry sees it. Only when every rung answered `None` is the
 * result `None`.
 */
object AnswerLadder {

  def firstAnswer[A](rungs: (() => Option[A])*): Option[A] = {
    val failures = Vector.newBuilder[Throwable]
    val outcomes = rungs.iterator.map(rung => try Right(rung()) catch { case NonFatal(failure) => Left(failure) })
    outcomes.map(_.left.map(failures += _)).collectFirst { case Right(Some(value)) => value }.orElse {
      failures.result() match {
        case first +: rest =>
          rest.foreach(first.addSuppressed)
          throw first
        case _ => None
      }
    }
  }
}

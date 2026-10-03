package tools

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.FiniteDuration

/**
 * A repository's read of Mongo, answered as the [[ReadOutcome]] it is: the document, its
 * absence, or a read that FAILED — never a failure dressed as "no such row".
 *
 * The shape this replaces is `Try(Await.result(query, timeout)).toOption.flatten`, which the
 * Mongo layer wrote again and again: a timeout, a lost primary or a refused credential came
 * back as `None`, byte-for-byte what a missing document gives, and the caller acted on the
 * absence — opened a change stream at "now" past every event since its saved position, or
 * read a venue as never archived. A wait that runs out is a failed read like any other.
 *
 * (`NoSwallowedRepositoryReadSpec` flags the old shape; whole-collection reads answer a
 * [[ScanOutcome]] through `services.movies.KeysetScan` instead.)
 */
object MongoRead {

  /** At most one document: [[ReadOutcome.Answered]] with it, [[ReadOutcome.Absent]] when there is
   *  none (`what` names it in the log), [[ReadOutcome.Failed]] when the read threw or timed out. */
  def one[A](what: => String, timeout: FiniteDuration)(query: => Future[Option[A]]): ReadOutcome[A] =
    apply(timeout)(query).flatMap(_.fold[ReadOutcome[A]](ReadOutcome.none(what))(ReadOutcome.Answered(_)))

  /** A read whose answer is whatever it returns — a page of documents, a count: answered or failed. */
  def apply[A](timeout: FiniteDuration)(query: => Future[A]): ReadOutcome[A] =
    ReadOutcome.of(Await.result(query, timeout))
}

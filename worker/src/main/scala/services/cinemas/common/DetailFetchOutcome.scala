package services.cinemas.common

import tools.HttpStatusException

import scala.util.{Failure, Success, Try}

/**
 * What a per-film detail fetch actually said, as `EnrichDetailsHandler` needs to
 * hear it: not just "did I get a detail" but — when I didn't — "is this worth
 * asking again soon".
 *
 * `Option[FilmDetail]` collapsed those two failures into one, and that collapse
 * was a livelock. A failed detail is deliberately NOT stamped fresh, so the
 * `DueWindow` never applies (`isDue` is unconditionally true with no stamp) and
 * `DetailReaper` re-enqueues the film on EVERY tick — once a minute, forever.
 * That is exactly right for a timeout and exactly wrong for a page the cinema
 * has taken down: the Cinema City chain row ran at ~90% failures on two films
 * whose detail pages had 404'd after their runs ended, drowning the real
 * enrichment failures on /uptime (a bucket keeps only 10 error strings).
 */
sealed trait DetailFetchOutcome

object DetailFetchOutcome {

  /** The detail came back. */
  final case class Fetched(detail: FilmDetail) extends DetailFetchOutcome

  /** The fetch failed for a reason that describes RIGHT NOW — a timeout, a 5xx,
   *  a rate limit, an unparseable body. Retry on the next reaper tick. */
  case object Failed extends DetailFetchOutcome

  /** The fetch failed for a reason that describes the URL — 404/410. Asking
   *  again buys the same answer, so the caller stamps it and backs off to the
   *  normal refresh window rather than retrying every tick. */
  final case class Gone(code: Int) extends DetailFetchOutcome

  /** Classify the result of a detail fetch. `None` and a thrown transient are
   *  both [[Failed]] — the every-tick retry every client has always had; only a
   *  durable [[HttpStatusException]] that a client lets ESCAPE becomes [[Gone]].
   *  So a client that still swallows its status keeps today's behaviour exactly,
   *  and opting in is a one-line change at that client's fetch. */
  def of(attempt: Try[Option[FilmDetail]]): DetailFetchOutcome = attempt match {
    case Success(Some(detail)) => Fetched(detail)
    case Success(None)         => Failed
    case Failure(failure: HttpStatusException) if HttpStatusException.isDurable(failure.code) => Gone(failure.code)
    case Failure(_)            => Failed
  }

  /** Run a detail-page fetch, swallowing a TRANSIENT failure into `None` — the
   *  "stay stale and retry" every client wants — while letting a DURABLE 404/410
   *  escape so [[of]] can see it. This is how a client opts into `Gone`
   *  classification; it replaces a bare `Try(...).toOption` around the fetch. */
  def transientToNone[A](fetch: => A): Option[A] = Try(fetch) match {
    case Success(value) => Some(value)
    case Failure(failure: HttpStatusException) if HttpStatusException.isDurable(failure.code) => throw failure
    case Failure(_)     => None
  }
}

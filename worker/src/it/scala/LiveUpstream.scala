package integration

import org.scalatest.Assertions.cancel

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

/**
 * Tells "the upstream is not answering right now" apart from "the upstream changed its
 * contract", so only the second one fails the build.
 *
 * The live specs cannot make that distinction themselves, because the clients under test
 * swallow every HTTP failure into `None` by design: a 429 from Rotten Tomatoes and a
 * genuine slug-convention change both arrive as `canonicalUrl(...) == None`, and the
 * assertion reads `None was not equal to Some(...)` either way. [[RetryWithBackoff]]
 * absorbs a burst, but a sustained block outlasts any budget, and then a third party's
 * rate limiter fails somebody else's deploy. Observed 2026-07-28: seven TMDB/IMDb cases
 * plus two Rotten Tomatoes ones failed together after a session of repeated local runs,
 * and all of them passed on the next attempt.
 *
 * So on failure, ask the upstream a question whose answer cannot be confused with the
 * assertion's: a plain GET of a stable URL on that host. 2xx means the site is up and
 * talking to us, so a `None` is a real regression and the failure stands. A throw —
 * timeout, 403, 429, 5xx — means it is not, and the case is CANCELLED rather than failed.
 *
 * A cancel is loud in the report and never red, which is the right trade: the cost of a
 * missed contract change is one cycle, the cost of a red build is everyone's deploy.
 */
object LiveUpstream {

  /** Probe URLs — chosen to be independent of anything under test: a site root or an
   *  endpoint that answers 2xx whenever the upstream is willing to talk to us at all. A
   *  probe must never be the call being asserted, or it would cancel the very regression
   *  it exists to let through. */
  object Probes {
    val Metacritic     = "https://www.metacritic.com/"
    val RottenTomatoes = "https://www.rottentomatoes.com/"
    val Imdb           = "https://www.imdb.com/"
    val Filmweb        = "https://www.filmweb.pl/"
    def tmdb(apiKey: String) = s"https://api.themoviedb.org/3/configuration?api_key=$apiKey"
  }

  /** Run `body` under the usual retry budget; if it still fails, classify the failure by
   *  probing `probe`. Reachable → rethrow. Unreachable → cancel.
   *
   *  The probe runs ONLY after a failure, so the happy path costs exactly what it did
   *  before. `sleep`/`now`/`totalBudget` are injectable for the same reason
   *  [[RetryWithBackoff]] injects them — so a spec can exercise the failure paths without
   *  sleeping through a real budget. */
  def orCancel[T](
    upstream:    String,
    probe:       () => Unit,
    stopwatch:   _root_.tools.Stopwatch,
    totalBudget: FiniteDuration = 30.seconds,
    sleep:       Long => Unit   = Thread.sleep
  )(body: => T): T =
    try RetryWithBackoff(stopwatch, totalBudget, sleep = sleep)(body)
    catch {
      // A TRANSPORT failure needs no probe, and must not be judged by one.
      //
      // The probe exists because the clients swallow HTTP failures into `None`, so a
      // failed assertion is ambiguous — a 429 and a changed slug convention read the
      // same. An exception that escaped the client is not ambiguous: no response was
      // ever received, so it cannot be a contract change, whatever a probe of the site
      // root then says. Deciding it by probe is how a POST to IMDb's GraphQL endpoint
      // timing out failed the build while `imdb.com/` answered 2xx a second later —
      // "the upstream is up" was true and beside the point.
      case transport: java.io.IOException =>
        cancel(
          s"$upstream did not answer at the transport level (${transport.getClass.getSimpleName}: " +
          s"${Option(transport.getMessage).getOrElse("")}) — cancelled rather than failed, because a " +
          "request that never got a response cannot tell us whether the contract changed.",
          transport)
      // A RATE LIMIT the call under test answered is as unambiguous: the endpoint said "slow down", which is no
      // contract change, and a probe of the site root answering 2xx says nothing about it. Landing check #933
      // (2026-11-15) went red on exactly this: IMDb's GraphQL endpoint 429'd the pipeline case's rating lookup while
      // `imdb.com/` answered the probe.
      case limited: _root_.tools.HttpStatusException if _root_.tools.HttpStatusException.isRateLimit(limited) =>
        cancel(
          s"$upstream rate-limited the call under test (${limited.getMessage}) — cancelled rather than failed, " +
          "because a throttle cannot tell us whether the contract changed.",
          limited)
      case NonFatal(failure) =>
        Try(probe()) match {
          case Success(_) => throw failure
          case Failure(unreachable) =>
            cancel(
              s"$upstream is not answering (${unreachable.getClass.getSimpleName}: " +
              s"${Option(unreachable.getMessage).getOrElse("")}) — cancelled rather than failed, " +
              s"because an unreachable upstream cannot tell us whether its contract changed. " +
              s"The assertion's own failure was: ${failure.getMessage}",
              failure)
        }
    }
}

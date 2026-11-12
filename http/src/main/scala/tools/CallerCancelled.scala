package tools

import java.util.concurrent.CancellationException

/** A call its CALLER abandoned rather than one the host failed. It can be the calling thread interrupted
 *  (`AdaptiveTimeoutScraper` cutting an overrun scrape, a worker stopping), or an async exchange cancelled
 *  (`RealHttpFetch.send` cancels the one in flight on that interrupt). It ended before the host answered, so it is no
 *  evidence about the host either way. It is not a failure to log against the host, not a breaker failure, not the
 *  host answering, and not an uptime sample; the outcome counter labels it [[HttpOutcome.Cancelled]]. Every
 *  decorator that judges a host asks this one question, so none of them takes a cancellation for an answer. */
object CallerCancelled {
  def apply(failure: Throwable): Boolean = failure match {
    case _: InterruptedException | _: CancellationException => true
    case _                                                  => false
  }
}

package services.cinemas

import java.io.IOException
import java.util.concurrent.TimeoutException

/** Classifies scrape failures. Network/HTTP errors from external cinema sites
 *  are expected and not actionable, so callers log them at WARN (invisible to
 *  Sentry) rather than ERROR. Used by the queue-driven ScrapeCinemaHandler. */
object ScrapeErrors {
  def isTransientHttpError(e: Throwable): Boolean = e match {
    case _: IOException      => true // timeouts, SSL, connection errors
    case _: TimeoutException => true // Future timeouts
    case e: RuntimeException =>
      val msg = Option(e.getMessage).getOrElse("")
      // "HTTP <code> for <method> <url>"          — RealHttpFetch.checkStatus
      // "All N backends failed for <verb> <url>"  — FallbackHttpFetch.tryEach
      msg.startsWith("HTTP ") ||
        (msg.startsWith("All ") && msg.contains(" backends failed for "))
    case _ => false
  }
}

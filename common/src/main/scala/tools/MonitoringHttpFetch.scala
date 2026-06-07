package tools

import services.UptimeMonitor

import java.io.IOException
import java.net.URI
import java.util.concurrent.CompletableFuture

/**
 *  Wraps an `HttpFetch` to feed every call's outcome to the `UptimeMonitor`,
 *  keyed by a service name derived from the request host (see `classify`).
 *
 *  `cinemaHosts` is the set of hosts that belong to a cinema scrape. They're
 *  SUPPRESSED here (recorded as `None`) because `RetryingCinemaScraper` already
 *  tracks each cinema's health under its `displayName`; a second per-host row
 *  would be a duplicate cluttering the uptime page's "Other" bucket. It's
 *  passed BY-NAME and memoised: the worker derives it from
 *  `CinemaScraperCatalog.scrapeHosts`, but the catalog itself fetches through
 *  this very `MonitoringHttpFetch`, so eager evaluation would be a constructor
 *  cycle. The thunk is forced once, on the first `classify` call — long after
 *  the wiring graph is built. Defaults to empty (the web wiring scrapes no
 *  cinemas, so it has nothing to suppress).
 */
class MonitoringHttpFetch(
  delegate: HttpFetch,
  monitor: UptimeMonitor,
  cinemaHosts: => Set[String] = Set.empty
) extends HttpFetch {

  private val enrichmentNames: Map[String, String] = Map(
    "api.themoviedb.org"        -> "TMDB",
    "caching.graphql.imdb.com"  -> "IMDb",
    "v3.sg.media-imdb.com"      -> "IMDb",
    "www.filmweb.pl"            -> "Filmweb",
    "filmweb.pl"                -> "Filmweb",
    "www.metacritic.com"        -> "Metacritic",
    "metacritic.com"            -> "Metacritic",
    "www.rottentomatoes.com"    -> "Rotten Tomatoes",
    "rottentomatoes.com"        -> "Rotten Tomatoes"
  )

  /** Force the by-name `cinemaHosts` exactly once, then reuse — building the
   *  catalog graph is not free, and `classify` runs per request. */
  private lazy val suppressedHosts: Set[String] = cinemaHosts

  private[tools] def classify(url: String): Option[String] =
    try {
      val host = URI.create(url).getHost
      // Enrichment FIRST: a host can be both an enrichment source and a scrape
      // source (filmweb.pl is scraped by FilmwebShowtimesClient and read for
      // ratings), and its enrichment health is what the uptime page wants — so
      // a named enrichment row wins over cinema-host suppression.
      enrichmentNames.get(host) match {
        case some @ Some(_)                         => some
        case None if suppressedHosts.contains(host) => None
        case None                                   => Some(host)
      }
    } catch {
      case _: Exception => None
    }

  private[tools] def isConnectionFailure(e: Throwable): Boolean = e match {
    case _: IOException      => true
    case e: RuntimeException => e.getMessage != null && e.getMessage.matches("HTTP 5\\d\\d .*")
    case _                   => false
  }

  private def errorDescription(e: Throwable): String = {
    val name = e.getClass.getSimpleName
    val msg  = Option(e.getMessage).getOrElse("")
    s"$name: $msg".take(200)
  }

  private def monitored[T](url: String)(block: => T): T = {
    classify(url) match {
      case None => block
      case Some(service) =>
        val t0 = System.nanoTime()
        def ms = (System.nanoTime() - t0) / 1000000L
        try {
          val result = block
          monitor.recordSuccess(service, ms)
          result
        } catch {
          case e: Exception =>
            if (isConnectionFailure(e)) monitor.recordFailure(service, errorDescription(e))
            else monitor.recordSuccess(service, ms)  // non-connection error → the call still reached the server
            throw e
        }
    }
  }

  override def get(url: String): String =
    monitored(url)(delegate.get(url))

  override def get(url: String, headers: Map[String, String]): String =
    monitored(url)(delegate.get(url, headers))

  override def getAsync(url: String): CompletableFuture[String] = {
    classify(url) match {
      case None => delegate.getAsync(url)
      case Some(service) =>
        val t0 = System.nanoTime()
        delegate.getAsync(url).whenComplete { (_, ex) =>
          val ms = (System.nanoTime() - t0) / 1000000L
          if (ex == null) monitor.recordSuccess(service, ms)
          else {
            val cause = unwrap(ex)
            if (isConnectionFailure(cause)) monitor.recordFailure(service, errorDescription(cause))
            else monitor.recordSuccess(service, ms)
          }
        }
    }
  }

  override def post(url: String, body: String, contentType: String): String =
    monitored(url)(delegate.post(url, body, contentType))

  private def unwrap(ex: Throwable): Throwable = ex match {
    case ce: java.util.concurrent.CompletionException if ce.getCause != null => ce.getCause
    case other => other
  }
}

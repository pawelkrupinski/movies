package services

import java.io.IOException
import java.net.URI
import java.util.concurrent.CompletableFuture

import tools.{CallerCancelled, ForwardingHttpFetch, HttpFetch, HttpStatusException, Stopwatch}

/**
 *  Wraps an `HttpFetch` to feed every call's outcome to the `UptimeMonitor`,
 *  keyed by a service name derived from the request host (see `classify`).
 *
 *  `cinemaHosts` is the set of hosts that belong to a cinema scrape. They're
 *  SUPPRESSED here (recorded as `None`) because `UptimeRecordingScraper` already
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
  monitor: UptimeRecorder,
  cinemaHosts: => Set[String] = Set.empty
) extends ForwardingHttpFetch {

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

  def classify(url: String): Option[String] =
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

  /** True when an exception means the call FAILED for uptime purposes: a
   *  transport/connection error (`IOException` — DNS, refused, socket/request
   *  timeout), a 5xx server error, OR a soft-block / throttle (HTTP 403
   *  Forbidden, 429 Too Many Requests). A 404 — and any other 4xx — is NOT a
   *  failure: several enrichment callers (Metacritic's slug probe, Rotten
   *  Tomatoes' URL probe) use 404 as a legitimate "this doesn't exist" signal,
   *  and the call DID reach the server. Counting 403/429 is what makes a
   *  Cloudflare / datacenter-IP soft-block VISIBLE on /uptime instead of
   *  silently recording as a success (which hid the Filmweb-block class of
   *  outage — a blocked enrichment source read green on the page). */
  def isFailure(e: Throwable): Boolean = e match {
    case _: IOException         => true
    case e: HttpStatusException => e.code >= 500 || e.code == 403 || e.code == 429
    case e: RuntimeException    => e.getMessage != null && e.getMessage.matches("HTTP (5\\d\\d|403|429) .*")
    case _                      => false
  }

  private def errorDescription(e: Throwable): String = {
    val name = e.getClass.getSimpleName
    val message  = Option(e.getMessage).getOrElse("")
    s"$name: $message".take(200)
  }

  protected def around[T](url: String)(call: HttpFetch => T): T = {
    classify(url) match {
      case None => call(delegate)
      case Some(service) =>
        val started = Stopwatch.start()
        try {
          val result = call(delegate)
          monitor.recordSuccess(service, started.millis)
          result
        } catch {
          case e: Exception => record(service, started.millis, Some(e)); throw e
        }
    }
  }

  override def getAsync(url: String): CompletableFuture[String] = {
    classify(url) match {
      case None => delegate.getAsync(url)
      case Some(service) =>
        val started = Stopwatch.start()
        delegate.getAsync(url).whenComplete((_, exception) => record(service, started.millis, Option(exception).map(unwrap)))
    }
  }

  /** One finished call's sample, `millis` after it started: a call abandoned by its caller before the service
   *  answered is none (neither up nor down); a [[isFailure]] is down; anything else -- a reply, or a 404 that
   *  reached the server -- is up. */
  private def record(service: String, millis: Long, failure: Option[Throwable]): Unit = failure match {
    case Some(e) if CallerCancelled(e) => ()
    case Some(e) if isFailure(e)       => monitor.recordFailure(service, errorDescription(e))
    case _                             => monitor.recordSuccess(service, millis)
  }

  private def unwrap(exception: Throwable): Throwable = exception match {
    case ce: java.util.concurrent.CompletionException if ce.getCause != null => ce.getCause
    case other => other
  }
}

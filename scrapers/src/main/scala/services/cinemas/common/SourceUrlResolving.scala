package services.cinemas.common

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try

/** A scraper whose canonical public page is resolved over the network rather than derived from its base URL — a
 *  Filmweb venue's `/showtimes/<City>/<Name>-<id>`, whose city and name only Filmweb's `/info` knows. Reached through
 *  [[CinemaScraper.sourceUrlResolving]], never a type test. */
trait SourceUrlResolving {
  def cinema: models.Cinema
  /** The canonical page, or `None` when it could not be resolved (the scraper's static `sourceUrl` stays). */
  def resolveSourceUrl(): Option[String]
}

/** Resolves the canonical /uptime link of every [[SourceUrlResolving]] scraper, in parallel on `executionContext`
 *  (the wiring bounds it, keeping us well under the upstream's soft limits) and tolerant — a venue whose resolve fails
 *  is simply absent, keeping its static `sourceUrl`; past `timeout` overall none is, so a slow or down upstream cannot
 *  stall worker boot. */
final class SourceUrlResolution(executionContext: ExecutionContext, timeout: FiniteDuration = 60.seconds) extends play.api.Logging {

  /** `cinema displayName -> canonical URL` for the resolvable scrapers among `scrapers`. */
  def resolveAll(scrapers: Seq[CinemaScraper]): Map[String, String] = {
    val resolving = scrapers.flatMap(_.sourceUrlResolving)
    if (resolving.isEmpty) Map.empty
    else {
      given ExecutionContext = executionContext
      val futures = resolving.map(r => Future(r.cinema.displayName -> r.resolveSourceUrl()))
      val pairs   = Try(Await.result(Future.sequence(futures), timeout))
        .getOrElse { logger.warn("source-url resolve timed out; using fallbacks"); Seq.empty }
      val resolved = pairs.collect { case (name, Some(url)) => name -> url }.toMap
      logger.debug(s"source URLs: resolved ${resolved.size}/${resolving.size} canonical links")
      resolved
    }
  }
}

package tools

import services.UptimeMonitor

import java.io.IOException
import java.net.URI
import java.util.concurrent.CompletableFuture

class MonitoringHttpFetch(delegate: HttpFetch, monitor: UptimeMonitor) extends HttpFetch {

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

  private val cinemaHosts: Set[String] = Set(
    "www.multikino.pl", "multikino.pl",
    "kinomalta.pl", "www.kinomalta.pl",
    "kinopalacowe.pl", "www.kinopalacowe.pl",
    "helios.pl", "www.helios.pl", "restapi.helios.pl",
    "www.cinema-city.pl", "cinema-city.pl",
    "www.kinomuza.pl", "kinomuza.pl",
    "kinobulgarska19.pl", "www.kinobulgarska19.pl",
    "kinoapollo.pl", "www.kinoapollo.pl",
    "www.kinorialto.poznan.pl", "kinorialto.poznan.pl"
  )

  private[tools] def classify(url: String): Option[String] =
    try {
      val host = URI.create(url).getHost
      if (cinemaHosts.contains(host)) None
      else Some(enrichmentNames.getOrElse(host, host))
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
        try {
          val result = block
          monitor.recordSuccess(service)
          result
        } catch {
          case e: Exception =>
            if (isConnectionFailure(e)) monitor.recordFailure(service, errorDescription(e))
            else monitor.recordSuccess(service)
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
        delegate.getAsync(url).whenComplete { (_, ex) =>
          if (ex == null) monitor.recordSuccess(service)
          else {
            val cause = unwrap(ex)
            if (isConnectionFailure(cause)) monitor.recordFailure(service, errorDescription(cause))
            else monitor.recordSuccess(service)
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

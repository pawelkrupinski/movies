package services.cinemas.common

import models.{Cinema, CinemaMovie}
import services.movies.ScrapeSink

/** A sink that takes the scrape and does nothing: for specs that pin what the runner itself does
 *  (archive, completeness), not what a landing does with the listing. */
object DiscardingScrapeSink extends ScrapeSink {
  def recordCinemaScrape(cinema: Cinema, movies: Seq[CinemaMovie], listingIsComplete: Boolean, sourceKey: Option[String],
                         viaFallback: Boolean): Unit = ()
}

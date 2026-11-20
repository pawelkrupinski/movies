package services.sharecards

import java.time.Clock

import models.{CityPath, Country}
import services.readmodel.{FilmSlugs, ReadModelReader}

/**
 * A film's page in every city it screens in, on the country's public origin — the URLs a
 * re-scrape asks Facebook to fetch again ([[FacebookRescrapeDrain]]).
 */
class FilmPageUrls(reader: ReadModelReader, country: Country, clock: Clock) extends (String => Seq[String]) {

  /** THROWS when the read model cannot be read — an empty list is "no page to refresh". The
   *  cities come from the film's own screenings ([[ReadModelReader.findCard]], a read by `_id`
   *  range), not a scan of every screening: a burst re-scrapes hundreds of films. */
  def apply(filmId: String): Seq[String] = {
    val card   = reader.findCard(filmId).required
    val cities = card.screenings.map(_._id.stripPrefix(s"$filmId|").takeWhile(_ != '|')).toSet
    val pagesIn = country.cities.filter(c => cities(c.slug))
    // A film screening nowhere (retired since its render) has no page: no slug read for it.
    if (pagesIn.isEmpty) Seq.empty
    else (for {
      s    <- slugFor(filmId).toSeq
      city <- pagesIn
      url  <- CityPath.filmUrl(city, s)
    } yield url).sorted
  }

  // A film's slug depends on every film (collisions are settled across the corpus), so it takes a
  // read of all of `web_movies`; a burst of re-scrapes (a template change re-draws every recent
  // film) shares one read for [[FilmPageUrls.SlugsFor]]. A film the read lacks re-reads.
  private val slugsOf = FilmSlugs.forCountry(country)
  private var slugs = Option.empty[(FilmSlugs, java.time.Instant)]
  private def slugFor(filmId: String): Option[String] = synchronized {
    val fresh = slugs.filter { case (_, at) => clock.instant().isBefore(at.plusMillis(FilmPageUrls.SlugsFor.toMillis)) }
      .map(_._1).filter(_.slugFor(filmId).isDefined)
    fresh.getOrElse {
      val read = slugsOf(reader.findAllMoviesChecked().required)
      slugs = Some(read -> clock.instant())
      read
    }.slugFor(filmId)
  }
}

object FilmPageUrls {
  import scala.concurrent.duration.*
  /** How long one read of the film slugs serves re-scrapes. */
  val SlugsFor: FiniteDuration = 10.minutes
}

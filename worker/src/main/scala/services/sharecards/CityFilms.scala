package services.sharecards

import java.time.{Clock, Instant}

import scala.concurrent.duration.*

import play.api.Logging
import services.readmodel.{ReadModelReader, ScreeningRef}

/** Each of `cities`' (slugs') films, the most widely shown first, for its city card ([[CityCardWriter]]).
 *
 *  From one read of the screenings' ids, kept for an hour: a render needs a city's films, and one `_id` scan an hour
 *  (the janitor's daily prune reads the same) is cheaper than a query per city on a collection with no city index.
 *  Held: each city's first [[CityFilms.Candidates]] film ids -- strings the ids already are -- so at most
 *  cities × 24 references. */
final class CityFilms(reader: ReadModelReader, cities: Set[String], clock: Clock) extends Logging {
  import CityFilms.{Candidates, FilmsMaxAge}

  private final case class Films(byCity: Map[String, Vector[String]], readAt: Instant)
  @volatile private var films: Option[Films] = None

  /** The city's films, the most widely shown first. Throws when no read has ever succeeded. */
  def of(citySlug: String): Vector[String] = {
    val now = clock.instant()
    val snapshot = synchronized {
      if (!films.exists(_.readAt.isAfter(now.minusMillis(FilmsMaxAge.toMillis))))
        reader.findAllScreeningRefsChecked().answered match {
          case Some(refs) => films = Some(Films(rank(refs), now))
          case None       => logger.warn("city cards: web_screenings read failed — drawing from the last read, if any.")
        }
      films.getOrElse(throw new IllegalStateException("no read of web_screenings has succeeded yet"))
    }
    snapshot.byCity.getOrElse(citySlug, Vector.empty)
  }

  private def rank(refs: Seq[ScreeningRef]): Map[String, Vector[String]] = {
    val venues = scala.collection.mutable.HashMap.empty[String, scala.collection.mutable.HashMap[String, Int]]
    refs.foreach(ref => ref.citySlug.filter(cities.contains).foreach { slug =>
      val counts = venues.getOrElseUpdate(slug, scala.collection.mutable.HashMap.empty)
      counts.update(ref.filmId, counts.getOrElse(ref.filmId, 0) + 1)
    })
    venues.iterator.map { case (slug, counts) =>
      slug -> counts.toVector.sortBy { case (film, n) => (-n, film) }.iterator.take(Candidates).map(_._1).toVector
    }.toMap
  }
}

object CityFilms {
  /** The films' ranking is re-read at most this often. */
  val FilmsMaxAge: FiniteDuration = 1.hour

  /** How many of a city's films are kept as poster candidates: twice the posters a card draws, as
   *  some have no poster cached. */
  val Candidates: Int = tools.CityCardRenderer.PosterCount * 2
}

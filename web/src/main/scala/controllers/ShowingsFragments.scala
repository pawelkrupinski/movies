package controllers

import java.time.LocalDate

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import models.Cinema
import services.metrics.CacheOccupancy

/**
 * A film's rendered showings tree (`ShowingsMarkup.days`), kept across renders of
 * its city's listing.
 *
 * WHY. A listing re-renders whenever its city's read-model stamp moves — New York's
 * every few minutes, and the synthetic probe re-fetches it — yet between two renders
 * almost every film's showings are unchanged: a stamp moves for one film's new slot,
 * and a slot lapsing changes one film. Rendering the tree is most of what a listing
 * costs (7 MB of New York's 7.7 MB page); kept, a re-render writes the unchanged
 * films from memory and renders only the ones that moved.
 *
 * THE KEY IS THE CONTENT, so a stale fragment cannot be served: anything the tree is
 * rendered from — the showings, the cinema links, the day its date labels count from,
 * the city (zone and language) — is in [[ShowingsFragments.Key]], and a film whose
 * showings changed asks for a different key. Superseded entries are never invalidated,
 * only aged out by the byte bound.
 */
trait ShowingsFragments {
  /** The fragment for `key`, rendering it with `render` only when it is not held. */
  def fragment(key: ShowingsFragments.Key)(render: => String): String
}

object ShowingsFragments {

  final case class Key(
    city: String,
    asOf: LocalDate,
    showings: Seq[(LocalDate, Seq[CinemaShowtimes])],
    cinemaFilmUrls: Seq[(Cinema, String)]
  )

  object Key {
    def of(film: FilmSchedule, city: models.City): Key =
      Key(city.slug, film.asOf, film.showings, film.cinemaFilmUrls)
  }

  /** Renders every time and keeps nothing — what every template gets unless a caller
   *  hands it a cache. `ShowingsMarkup` streams the tree for this one instead of
   *  building it as a string, since nothing will keep the string. */
  object Uncached extends ShowingsFragments {
    def fragment(key: Key)(render: => String): String = render
  }
}

/** [[ShowingsFragments]] in a byte-bounded Caffeine cache: least recently used out
 *  first, each fragment weighed by the bytes its string holds. */
final class CaffeineShowingsFragments(maxBytes: Long) extends ShowingsFragments {

  private val cache: Cache[ShowingsFragments.Key, String] =
    Caffeine.newBuilder()
      .maximumWeight(maxBytes)
      .weigher[ShowingsFragments.Key, String]((_, fragment) => CaffeineShowingsFragments.bytesHeld(fragment))
      .recordStats()
      .build[ShowingsFragments.Key, String]()

  def fragment(key: ShowingsFragments.Key)(render: => String): String =
    cache.get(key, _ => render)

  def occupancy: CacheOccupancy = CacheOccupancy.of(cache, weighted = true)
}

object CaffeineShowingsFragments {

  /** Enough for New York's whole listing (~7 MB of fragments) and the next cities'
   *  several times over; the response cache beside it holds 64 MB of gzipped pages. */
  val DefaultMaxBytes: Long = 16L * 1024 * 1024

  /** A string's bytes: one per char while every char is Latin-1, two for all of them
   *  once one is not (Java's compact strings). */
  private def bytesHeld(fragment: String): Int =
    if (fragment.chars().allMatch(_ <= 0xFF)) fragment.length else fragment.length * 2
}

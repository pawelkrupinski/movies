package controllers

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import play.twirl.api.Html
import services.metrics.CacheOccupancy

/**
 * A film's rendered listing card — its poster, links, ratings and whole showings tree —
 * kept across renders of its city's listing.
 *
 * WHY. A listing re-renders whenever its city's read-model stamp moves — New York's
 * every few minutes, and the synthetic probe re-fetches it — yet between two renders
 * almost every film is unchanged: a stamp moves for one film's new slot, and a slot
 * lapsing changes one film. The cards are the page (7.4 MB of New York's 7.7 MB);
 * kept, a re-render writes the unchanged ones from memory and renders only the films
 * that moved. On the live pod the showings alone hit 98.9% of the time.
 *
 * THE KEY IS THE CONTENT, so a stale card cannot be served: the card is rendered from
 * the film's `FilmSchedule` (metadata, ratings, showings, cinema links, the day its
 * dates count from), its city (zone and language) and the deployment's messages, and
 * all of them are the key. A film that changed in any of them asks for another key;
 * superseded entries are never invalidated, only aged out by the byte bound.
 */
trait FilmCardFragments {
  /** The card for `key`, rendering it with `render` only when it is not held. */
  def fragment(key: FilmCardFragments.Key)(render: => String): String
}

object FilmCardFragments {

  final case class Key(city: String, language: String, film: FilmSchedule)

  /** Renders every time and keeps nothing — what every template gets unless a caller
   *  hands it a cache. Its cards are written straight into the body (their showings
   *  streamed), never held as strings, since nothing would keep them. */
  object Uncached extends FilmCardFragments {
    def fragment(key: Key)(render: => String): String = render
  }

  /** `card`, from `fragments` when it holds it, rendered once into the string it keeps
   *  when it does not. */
  def cached(film: FilmSchedule, city: models.City, fragments: FilmCardFragments)(card: => Html)
            (implicit messages: play.api.i18n.Messages): Html =
    fragments match {
      case Uncached => card
      case cache =>
        val text = cache.fragment(Key(city.slug, messages.lang.code, film))(card.body)
        // Wrapped in a plain `Html`: see `StreamedHtml` on why a template must never
        // be handed a subclass. Written straight from the kept string (`PrewrittenHtml`).
        new Html(List(new PrewrittenHtml(text)))
    }
}

/** [[FilmCardFragments]] in a byte-bounded Caffeine cache: least recently used out
 *  first, each card weighed by the bytes its string holds. */
final class CaffeineFilmCardFragments(maxBytes: Long) extends FilmCardFragments {

  private val cache: Cache[FilmCardFragments.Key, String] =
    Caffeine.newBuilder()
      .maximumWeight(maxBytes)
      .weigher[FilmCardFragments.Key, String]((_, card) => CaffeineFilmCardFragments.bytesHeld(card))
      .recordStats()
      .build[FilmCardFragments.Key, String]()

  def fragment(key: FilmCardFragments.Key)(render: => String): String =
    cache.get(key, _ => render)

  def occupancy: CacheOccupancy = CacheOccupancy.of(cache, weighted = true)
}

object CaffeineFilmCardFragments {

  /** New York's whole listing (~7.4 MB of cards) and the next cities' several times
   *  over; the response cache beside it holds 64 MB of gzipped pages. */
  val DefaultMaxBytes: Long = 24L * 1024 * 1024

  /** A string's bytes: one per char while every char is Latin-1, two for all of them
   *  once one is not (Java's compact strings). */
  private def bytesHeld(card: String): Int =
    if (card.chars().allMatch(_ <= 0xFF)) card.length else card.length * 2
}

package services.cinemas

import models.CinemaMovie

/** Mix into a cinema client whose feed includes non-film live events (concerts,
 *  stand-up, kabaret, recitals, theatre plays) alongside its films — small
 *  municipal/arthouse venues sell tickets to both through one listing. The
 *  client implements [[fetchUnfiltered]] (its raw scrape) instead of `fetch`;
 *  this trait owns the public `fetch` and drops the events via
 *  [[NonMovieEventClassifier]].
 *
 *  Template-method shape (not a stackable `abstract override def fetch`):
 *  because each client defines its scrape in its own class body, a class-level
 *  `abstract override def fetch` would be shadowed by that leaf method
 *  (linearization), so `super.fetch` would never run. Routing the client's
 *  scrape through `fetchUnfiltered` keeps `fetch` the single public method, so
 *  it still composes with the `Retrying`/`Uptime`/`FilmwebFallback` wrappers
 *  (they delegate `fetch()`).
 *
 *  High-precision by design — distributed "event cinema" broadcasts (André
 *  Rieu, National Theatre Live, Royal Ballet & Opera, Pavarotti) and art
 *  documentaries are deliberately kept; see [[NonMovieEventClassifier]]. An
 *  all-events tick collapses `fetch()` to empty, which
 *  `MovieCache.recordCinemaScrape` already treats like a failed/empty tick
 *  (keeps the cinema's existing slots rather than pruning them). */
trait OnlyMovieEventsFilter extends CinemaScraper {

  /** The client's raw scrape. Implement this instead of `fetch()`. */
  protected def fetchUnfiltered(): Seq[CinemaMovie]

  final def fetch(): Seq[CinemaMovie] =
    fetchUnfiltered().filterNot(cm => NonMovieEventClassifier.isLiveEvent(cm.movie.title))
}

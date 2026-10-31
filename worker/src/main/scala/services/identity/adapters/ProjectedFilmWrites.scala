package services.identity
package adapters

import services.movies.{CacheKey, FilmId, MovieCache, WriteOutcome}
import models.MovieRecord

/** The identity projection's writes of its planned films to the store ([[IdentityProjection]]): their order under the
 *  store's unique indexes, the independent ones side by side, a cycle broken by parking, retirements between. Logs under
 *  the projection's `logger`. */
private[identity] final class ProjectedFilmWrites(cache: MovieCache, logger: play.api.Logger) {

  /** Write `films` and retire `retired`, in the order the store's unique `key` / `tmdbId` indexes
   *  allow: a film whose key or TMDB id another document still holds waits until the retirements
   *  and the other writes have freed it; a cycle between two kept films (each holding the other's
   *  key) is broken by parking one under a key and id of its own first. Returns the films not written as planned
   *  (declined, or parked without their TMDB id) — the next projection tries them again — and, of those, the ones the
   *  store holds parked, as it holds them.
   *
   *  The `independent` films — new ones whose key and TMDB id nothing else holds or takes — are
   *  written side by side first: no order among them, or with the rest, can decide anything, and
   *  one at a time a country's first projection waited on ~2,250 films' round-trips in a row (a US
   *  boot). The rest keep the plan's order. */
  def apply(films: Seq[ProjectedFilm], retired: Seq[FilmId], independent: Set[FilmId],
            patchable: ProjectedFilm => Option[MovieRecord]): (Set[FilmId], Seq[ProjectedFilm]) = {
    def write(f: ProjectedFilm): Boolean = {
      val key = CacheKey.stored(f.title, f.key)
      patchable(f).fold(cache.writeProjected(f.id, key, f.record))(cache.patchProjected(f.id, key, _, f.record, f.touched)) == WriteOutcome.Written
    }
    def attempt(fs: Seq[ProjectedFilm]): Seq[ProjectedFilm] = fs.filterNot(write)
    val (apart, ordered) = films.partition(f => independent(f.id))
    val unwritten = tools.BoundedParallel.map("identity-projection-writes", apart, IdentityProjection.WriteConcurrency)(f => Option.unless(write(f))(f))
    val first = unwritten.flatten ++ attempt(ordered)
    retired.foreach { id =>
      val outcome = cache.retireProjected(id)
      if (outcome != WriteOutcome.Written) tools.LogFields.warn(logger.underlyingLogger, s"identity projection: retiring $id: $outcome",
        Seq("film" -> id.value, "outcome" -> outcome.toString))
    }
    val second = attempt(first)
    val parked = second.map(f => f.copy(key = s"~${f.id.value}|", record = f.record.copy(tmdbId = None), touched = None))
      .filter(f => cache.writeProjected(f.id, CacheKey.stored(f.title, f.key), f.record) == WriteOutcome.Written)
    val last = attempt(second)
    if (last.nonEmpty) tools.LogFields.warn(logger.underlyingLogger, s"identity projection: ${last.size} film(s) not written " +
      s"(e.g. ${last.head.id} under ${last.head.key}); ${parked.size} parked; the next projection retries",
      Seq("declined" -> last.size, "parked" -> parked.size,
        "declined_films" -> last.take(IdentityProjection.FilmsNamed).map(f => s"${f.id.value}=${f.key}").mkString(" ")))
    val declined = last.map(_.id).toSet
    (declined, parked.filter(f => declined(f.id)))
  }
}

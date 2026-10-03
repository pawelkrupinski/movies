package services.readmodel

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

/**
 * Film id -> the cities screening it, so a movie document's change bumps only those cities'
 * validators. Deliberately a SUPERSET between rebuilds: a screening delete cannot tell whether the
 * city still shows the film at another venue, so a pair is added but never removed incrementally,
 * and [[rebuild]] makes it exact on every complete reload. Drift therefore only over-invalidates.
 */
private[readmodel] final class FilmCities {
  private val cities = new ConcurrentHashMap[String, java.util.Set[String]]()

  def of(filmId: String): Seq[String] = Option(cities.get(filmId)).fold(Seq.empty[String])(_.asScala.toSeq)

  // Atomic per film with `rebuild`'s pruning of it, so an add never lands in a set just dropped.
  def add(filmId: String, city: String): Unit = {
    cities.compute(filmId, (_, held) => {
      val set = if (held == null) ConcurrentHashMap.newKeySet[String]() else held
      set.add(city)
      set
    })
    ()
  }

  /** Make the index exactly `next`, plus every pair `keep` vouches for. Pruned in place, never
   *  cleared: a pair `next` holds is present throughout, so a movie change applied mid-rebuild still
   *  bumps that film's cities — cleared and refilled, it found none in between and bumped nothing,
   *  leaving those cities' cached pages stale behind a 304. */
  def rebuild(next: java.util.Map[String, java.util.Set[String]], keep: (String, String) => Boolean): Unit = {
    cities.keySet.forEach { filmId =>
      cities.computeIfPresent(filmId, (_, held) => {
        val wanted = Option(next.get(filmId))
        held.removeIf(city => !wanted.exists(_.contains(city)) && !keep(filmId, city))
        if (held.isEmpty) null else held
      })
      ()
    }
    addAll(next)
  }

  def addAll(next: java.util.Map[String, java.util.Set[String]]): Unit =
    next.forEach((filmId, set) => set.forEach(city => add(filmId, city)))
}

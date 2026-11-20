package services.corpus

import java.time.Clock
import java.util.concurrent.ConcurrentHashMap

import scala.jdk.CollectionConverters.*
import scala.util.Try

import models.{CacheKey, City, StoredMovieRecord}
import play.api.Logging
import services.evidence.LatestTitleYear
import services.titles.TitleNormalizer

/** Each film's part in the [[CorpusCensus]] ([[FilmCensus]]), as of the last drain, and the time-free subsets'
 *  running sums over them; and the films the cache changed since, noted for the next drain to derive. Keyed by `K`
 *  (the cache's key, `describe`d in a log line), each film an `F` its part is derived from (`derive`, given its last). */
private[corpus] final class CensusParts[K, F](derive: (F, Option[FilmCensus]) => FilmCensus, describe: K => String,
                                              cities: Seq[City], clock: Clock) extends Logging {

  // Both guarded by `parts`.
  private val parts  = scala.collection.mutable.HashMap.empty[K, FilmCensus]
  private val static = new Array[Long](CensusSubsets.StaticNames.size)
  // The films the cache changed since the last drain, each as it now holds it (`None`: gone).
  private val dirty  = new ConcurrentHashMap[K, Option[F]]()

  /** Note the film `key` now holds (`None`: gone), for the next drain to derive. Called under the cache's lock for
   *  the key, on the writer's thread — so it only notes: deriving a film's part is linear in its slots, and a wide
   *  release at 3,300 venues was re-derived whole for each venue's landing, inside that lock. */
  def held(key: K, film: Option[F]): Unit = { dirty.put(key, film); () }

  /** The census as the films held now give it, against the clock now, and whether every film noted since the last
   *  drain could be counted. */
  def drainedReading(): (CensusReading, Boolean) = parts.synchronized {
    val whole = drain()
    (CensusReading.tally(parts.valuesIterator, static.toSeq, cities, clock), whole)
  }

  /** Derive the part of every film noted since the last drain, in place of its last one. A film whose derivation
   *  throws keeps its last part and stays noted, so the next drain tries it again rather than leaving it wrong until
   *  its next write. Whether every film derived. Caller holds `parts`. */
  private def drain(): Boolean = dirty.keySet().asScala.toSeq.foldLeft(true) { (whole, key) =>
    Option(dirty.remove(key)).fold(whole) { film =>
      val prior = parts.get(key)
      Try(film.map(derive(_, prior))) match {
        case scala.util.Success(next) =>
          next.fold(parts.remove(key))(parts.put(key, _))
          prior.foreach(p => add(p.subsets, -1))
          next.foreach(n => add(n.subsets, +1))
          whole
        case scala.util.Failure(e) =>
          dirty.putIfAbsent(key, film)
          logger.warn(s"corpus-census: could not count '${describe(key)}' (${e.getMessage}) — it keeps its last count until the next tick.")
          false
      }
    }
  }

  private def add(subsets: Int, by: Int): Unit =
    CensusSubsets.StaticNames.indices.foreach(i => if ((subsets & (1 << i)) != 0) static(i) += by)
}

private[corpus] object CensusParts {
  /** The parts of the cache's films: each derived under `normalizer`'s title rules, with the latest year a title may
   *  name read off `clock` at each ask (`LatestTitleYear`), and described by its clean title. */
  def ofFilms(normalizer: TitleNormalizer, cities: Seq[City], clock: Clock): CensusParts[CacheKey, StoredMovieRecord] = {
    given LatestTitleYear = LatestTitleYear(clock)
    new CensusParts[CacheKey, StoredMovieRecord](FilmCensus.of(_, normalizer, _), _.cleanTitle, cities, clock)
  }
}

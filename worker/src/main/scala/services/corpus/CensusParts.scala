package services.corpus

import models.City
import play.api.Logging
import services.movies.{CacheKey, LatestTitleYear, StoredMovieRecord}
import services.titles.TitleNormalizer

import java.time.Clock
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._
import scala.util.Try

/** Each film's part in the [[CorpusCensus]] ([[FilmCensus]]), as of the last drain, and the time-free subsets'
 *  running sums over them; and the films the cache changed since, noted for the next drain to derive. */
private[corpus] final class CensusParts(normalizer: TitleNormalizer, cities: Seq[City], clock: Clock) extends Logging {
  // The latest year a title may name, read off this class's clock at each ask (`LatestTitleYear`).
  private given LatestTitleYear = LatestTitleYear(clock)

  // Both guarded by `parts`.
  private val parts  = scala.collection.mutable.HashMap.empty[CacheKey, FilmCensus]
  private val static = new Array[Long](CensusSubsets.Static.size)
  // The films the cache changed since the last drain, each as it now holds it (`None`: gone).
  private val dirty  = new ConcurrentHashMap[CacheKey, Option[StoredMovieRecord]]()

  /** Note the film `key` now holds (`None`: gone), for the next drain to derive. Called under the cache's lock for
   *  the key, on the writer's thread — so it only notes: deriving a film's part is linear in its slots, and a wide
   *  release at 3,300 venues was re-derived whole for each venue's landing, inside that lock. */
  def held(key: CacheKey, film: Option[StoredMovieRecord]): Unit = { dirty.put(key, film); () }

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
      Try(film.map(FilmCensus.of(_, normalizer, prior))) match {
        case scala.util.Success(next) =>
          next.fold(parts.remove(key))(parts.put(key, _))
          prior.foreach(p => add(p.subsets, -1))
          next.foreach(n => add(n.subsets, +1))
          whole
        case scala.util.Failure(e) =>
          dirty.putIfAbsent(key, film)
          logger.warn(s"corpus-census: could not count '${key.cleanTitle}' (${e.getMessage}) — it keeps its last count until the next tick.")
          false
      }
    }
  }

  private def add(subsets: Int, by: Int): Unit =
    CensusSubsets.Static.indices.foreach(i => if ((subsets & (1 << i)) != 0) static(i) += by)
}

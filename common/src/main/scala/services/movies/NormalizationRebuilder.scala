package services.movies

import models.{Cinema, MovieRecord, Source, SourceData}
import play.api.Logging
import services.titlerules.TitleRuleKey

/** Re-derives every row's merge key from its cinema slots' RAW titles under the
 *  currently-installed rules, then rebuilds the cache so rows group exactly as a
 *  fresh scrape would. This is the apply side of a rule change: after the change
 *  stream installs an edited rule set, the worker runs this so EXISTING records
 *  re-merge (a rule was added) AND un-merge (a rule was removed), not just
 *  future scrapes.
 *
 *  Unified model: explode each record into one fragment per distinct target key
 *  of its cinema slots, then regroup all fragments by key and union each group.
 *   - A row whose slots all share a key → one fragment → unchanged or merged
 *     with others that now share it (ADD case).
 *   - A row whose slots diverge onto N keys → N fragments → it SPLITS (DELETE
 *     case). The fragment under the original key keeps the enrichment; the
 *     split-offs are fresh (no tmdbId) and handed to `onSplitOff` for
 *     re-resolution.
 *
 *  Best-effort and idempotent: a second run over a consistent cache writes
 *  nothing. Operates on the live cache (write-through to Mongo). */
class NormalizationRebuilder(
  cache: MovieCache,
  // Called with the (cleanTitle, year) of each split-off / newly-unenriched row
  // so the worker can re-resolve its TMDB id + ratings (typically by publishing
  // MovieRecordCreated). No-op by default (tests, web). Exposes primitives, not
  // the package-private CacheKey, so the composition root can wire it.
  onSplitOff: (String, Option[Int]) => Unit = (_, _) => ()
) extends Logging {
  import NormalizationRebuilder._

  private case class Frag(key: CacheKey, rec: MovieRecord, from: CacheKey, fresh: Boolean)

  private def keyOfSlot(cinema: Cinema, slot: SourceData, year: Option[Int]): CacheKey = {
    val raw = slot.rawTitle.orElse(slot.title).getOrElse("")
    cache.keyOf(TitleNormalizer.cinemaClean(TitleRuleKey.of(cinema), raw), year)
  }

  private def fragmentsOf(oldKey: CacheKey, rec: MovieRecord): Seq[Frag] = {
    val byKey: Map[CacheKey, Seq[(Cinema, SourceData)]] =
      rec.cinemaData.toSeq.groupBy { case (cinema, slot) => keyOfSlot(cinema, slot, oldKey.year) }
    if (byKey.sizeIs <= 1) {
      Seq(Frag(byKey.keys.headOption.getOrElse(oldKey), rec, oldKey, fresh = false))
    } else {
      // Non-cinema (Tmdb/Imdb) slots + the record's top-level enrichment stay
      // with the remnant: the fragment under the original key, else the largest.
      val nonCinema: Map[Source, SourceData] = rec.data -- rec.cinemaData.keys
      val remnant   = if (byKey.contains(oldKey)) oldKey else byKey.maxBy(_._2.size)._1
      byKey.toSeq.map { case (k, pairs) =>
        val slots: Map[Source, SourceData] = pairs.map { case (c, s) => (c: Source) -> s }.toMap
        if (k == remnant) Frag(k, rec.copy(data = slots ++ nonCinema), oldKey, fresh = false)
        else              Frag(k, MovieRecord(data = slots),           oldKey, fresh = true)
      }
    }
  }

  def rebuild(): RebuildResult = {
    val records = cache.entries
    val frags   = records.flatMap { case (k, r) => fragmentsOf(k, r) }
    val byKey   = frags.groupBy(_.key)

    var changed = 0
    // Keys that no longer host any fragment (a merge victim, or a fully-moved row).
    (records.map(_._1).toSet -- byKey.keySet).foreach { k => cache.invalidate(k); changed += 1 }

    val merges = scala.collection.mutable.ListBuffer.empty[MergeEvent]
    byKey.foreach { case (key, fs) =>
      val recs      = fs.map(_.rec)
      val canonical = recs.find(_.tmdbId.isDefined).getOrElse(recs.head)
      val merged    = recs.filterNot(_ eq canonical).foldLeft(canonical)(MovieRecordMerge.union)
      if (!cache.get(key).contains(merged)) { cache.put(key, merged); changed += 1 }
      if (fs.map(_.from).distinct.sizeIs > 1)
        merges += MergeEvent(merged.displayTitle(key.cleanTitle), key.year,
          fs.flatMap(_.rec.cinemaTitles).distinct.sorted)
      if (merged.tmdbId.isEmpty && fs.exists(_.fresh)) onSplitOff(key.cleanTitle, key.year)
    }

    val splits = frags.groupBy(_.from).iterator.collect {
      case (from, fs) if fs.map(_.key).distinct.sizeIs > 1 =>
        SplitEvent(from.cleanTitle, fs.map(f => f.rec.displayTitle(f.key.cleanTitle)).distinct.sorted)
    }.toSeq

    val result = RebuildResult(changed, merges.toSeq, splits)
    logger.info(s"NormalizationRebuilder: changed=$changed, merges=${result.merges.size}, splits=${result.splits.size}")
    result
  }
}

object NormalizationRebuilder {
  /** Rows that collapsed into one. */
  final case class MergeEvent(display: String, year: Option[Int], mergedTitles: Seq[String])
  /** A row that split into several. */
  final case class SplitEvent(from: String, into: Seq[String])
  final case class RebuildResult(changed: Int, merges: Seq[MergeEvent], splits: Seq[SplitEvent])
}

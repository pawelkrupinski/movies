package services.movies

import models.Cinema
import play.api.Logging
import services.titlerules.TitleRuleKey

/** Re-derives every row's merge key from its cinema slots' RAW titles under the
 *  currently-installed rules, and collapses rows that now share a key. This is
 *  the apply side of a rule change: after the change stream installs an edited
 *  rule set, the worker runs this so EXISTING records merge the same way a fresh
 *  scrape would — not just future scrapes.
 *
 *  Operates on the live cache (write-through to Mongo). MERGE only: a row whose
 *  cinema slots now diverge onto different keys is flagged `needsSplit` and left
 *  in place for the un-merge phase. Best-effort and idempotent — a second run
 *  over an already-consistent cache is a no-op. */
class NormalizationRebuilder(cache: MovieCache) extends Logging {
  import NormalizationRebuilder._

  /** The merge key a row's cinema slots map to under the current rules: the
   *  distinct set of per-slot keys (1 → coherent, >1 → needs split). */
  private def targetKeys(oldKey: CacheKey, rec: models.MovieRecord): Seq[CacheKey] =
    rec.cinemaData.toSeq.flatMap { case (cinema: Cinema, slot) =>
      slot.rawTitle.orElse(slot.title).filter(_.nonEmpty).map { raw =>
        cache.keyOf(TitleNormalizer.cinemaClean(TitleRuleKey.of(cinema), raw), oldKey.year)
      }
    }.distinct

  def rebuild(): RebuildResult = {
    val records = cache.entries
    val classified = records.map { case (oldKey, rec) =>
      val keys   = targetKeys(oldKey, rec)
      val newKey = keys match { case Seq(single) => single; case _ => oldKey } // 0 or >1 → keep
      (oldKey, rec, newKey, keys.sizeIs > 1)
    }
    val needsSplit = classified.collect { case (k, _, _, true) => k.cleanTitle }

    var rekeyed = 0
    val merges  = scala.collection.mutable.ListBuffer.empty[MergeEvent]

    classified.groupBy(_._3).foreach { case (newKey, group) =>
      val singleNoMove = group.sizeIs == 1 && group.head._1 == newKey
      if (!singleNoMove) {
        val recs = group.map(_._2)
        // Canonical: the row already at newKey, else the most-enriched (tmdbId),
        // so union keeps the richest top-level fields (ratings, ids).
        val canonical = group.find(_._1 == newKey).map(_._2)
          .orElse(recs.find(_.tmdbId.isDefined))
          .getOrElse(recs.head)
        val merged = recs.filterNot(_ eq canonical).foldLeft(canonical)(MovieRecordMerge.union)
        group.foreach { case (oldKey, _, _, _) =>
          if (oldKey != newKey) { cache.invalidate(oldKey); rekeyed += 1 }
        }
        cache.put(newKey, merged)
        if (group.sizeIs > 1)
          merges += MergeEvent(merged.displayTitle(newKey.cleanTitle), newKey.year,
            group.flatMap(_._2.cinemaTitles).distinct.sorted)
      }
    }

    val result = RebuildResult(rekeyed, merges.toSeq, needsSplit)
    logger.info(s"NormalizationRebuilder: rekeyed=${result.rekeyed}, merges=${result.merges.size}, needsSplit=${needsSplit.size}")
    result
  }
}

object NormalizationRebuilder {
  /** A set of rows that collapsed into one during a rebuild. */
  final case class MergeEvent(display: String, year: Option[Int], mergedTitles: Seq[String])
  final case class RebuildResult(rekeyed: Int, merges: Seq[MergeEvent], needsSplit: Seq[String])
}

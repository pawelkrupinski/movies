package services.movies

import models.{Cinema, MovieRecord, Source, SourceData}
import play.api.Logging
import services.titlerules.{TitleRuleKey, TitleRuleSet}

/** Re-derives every row's merge key from its cinema slots' cleaned titles under
 *  the currently-installed rules (see `keyOfSlot` — the same `cinemaClean(title)`
 *  a fresh scrape keys by), then rebuilds the cache so rows group exactly as a
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
  // MovieDetailsComplete). No-op by default (tests, web). Exposes primitives, not
  // the package-private CacheKey, so the composition root can wire it.
  onSplitOff: (String, Option[Int]) => Unit = (_, _) => (),
  // Prometheus sinks so a rule-change re-merge / un-merge wave shows on the same
  // panels as the runtime folds (merges by reason `normalize-rebuild`, plus the
  // splits counter). No-op for web / unit tests.
  mergeMetrics: MergeMetrics = MergeMetrics.noop,
  splitMetrics: SplitMetrics = SplitMetrics.noop
) extends Logging {
  import NormalizationRebuilder._

  private case class Frag(key: CacheKey, record: MovieRecord, from: CacheKey, fresh: Boolean)

  /** Re-derive a slot's merge key the SAME way a fresh scrape does:
   *  `keyOf(cinemaClean(slot.title))`. `recordCinemaScrape` keys a slot by its
   *  CLEANED title (`SourceData.title` = `cinemaClean(client title)`), NOT by the
   *  verbatim `rawTitle`. Some clients strip format/language decoration ("DUB",
   *  "2D DUBBING", "2D NAPISY") in their own parsing — that lives in `title`, not
   *  in any title-rule — so re-keying off `rawTitle` re-introduces it and
   *  fragments rows a fresh scrape keeps merged. Prefer `title`; fall back to
   *  `rawTitle`, then to the row's own key (a blank or titleless slot must never
   *  spawn its own empty-keyed row). */
  private def keyOfSlot(cinema: Cinema, slot: SourceData, oldKey: CacheKey): CacheKey = {
    def keyFrom(raw: String): CacheKey =
      cache.keyOf(TitleNormalizer.cinemaClean(TitleRuleKey.of(cinema), raw), oldKey.year)
    (slot.title.iterator ++ slot.rawTitle.iterator)
      .map(keyFrom)
      .find(k => TitleNormalizer.sanitize(k.cleanTitle).nonEmpty)
      .getOrElse(oldKey)
  }

  private def fragmentsOf(oldKey: CacheKey, record: MovieRecord): Seq[Frag] = {
    val byKey: Map[CacheKey, Seq[(Cinema, SourceData)]] =
      record.cinemaData.toSeq.groupBy { case (cinema, slot) => keyOfSlot(cinema, slot, oldKey) }
    if (byKey.sizeIs <= 1) {
      Seq(Frag(byKey.keys.headOption.getOrElse(oldKey), record, oldKey, fresh = false))
    } else {
      // Non-cinema (Tmdb/Imdb/Filmweb) slots + the record's top-level enrichment stay
      // with the remnant: the fragment under the original key, else the largest.
      val nonCinema: Map[Source, SourceData] = record.data -- record.cinemaData.keys
      val remnant   = if (byKey.contains(oldKey)) oldKey else byKey.maxBy(_._2.size)._1
      byKey.toSeq.map { case (k, pairs) =>
        val slots: Map[Source, SourceData] = pairs.map { case (c, s) => (c: Source) -> s }.toMap
        if (k == remnant) Frag(k, record.copy(data = slots ++ nonCinema), oldKey, fresh = false)
        else              Frag(k, MovieRecord(data = slots),           oldKey, fresh = true)
      }
    }
  }

  def rebuild(): RebuildResult = {
    val records = cache.entries
    val frags   = records.flatMap { case (k, r) => fragmentsOf(k, r) }
      // An empty merge key is never a real film row. `keyOfSlot` keeps live
      // blank-raw slots on their record's key, but a row whose key ITSELF
      // sanitises to empty (a pre-existing phantom from an all-blank slot) has
      // no valid home — drop it so the rebuild prunes the junk instead of
      // re-persisting it.
      .filter(f => TitleNormalizer.sanitize(f.key.cleanTitle).nonEmpty)
    val byKey   = frags.groupBy(_.key)

    var changed = 0
    // Keys that no longer host any fragment (a merge victim, a fully-moved row,
    // or an empty-keyed phantom dropped above).
    (records.map(_._1).toSet -- byKey.keySet).foreach { k => cache.invalidate(k); changed += 1 }

    val merges = scala.collection.mutable.ListBuffer.empty[MergeEvent]
    byKey.foreach { case (key, fs) =>
      val merged = MovieRecordMerge.unionAll(fs.map(_.record))
      if (!cache.get(key).contains(merged)) { cache.put(key, merged); changed += 1 }
      val sources = fs.map(_.from).distinct
      if (sources.sizeIs > 1) {
        merges += MergeEvent(merged.displayTitle(key.cleanTitle), key.year,
          fs.flatMap(_.record.cinemaTitles).distinct.sorted)
        // One increment per source row absorbed beyond the survivor — the same
        // victims-counting convention as the runtime folds.
        mergeMetrics.recordMerge(MergeReason.NormalizeRebuild, sources.size - 1)
      }
      if (merged.tmdbId.isEmpty && fs.exists(_.fresh)) onSplitOff(key.cleanTitle, key.year)
    }

    val splits = frags.groupBy(_.from).iterator.collect {
      case (from, fs) if fs.map(_.key).distinct.sizeIs > 1 =>
        val into = fs.map(f => f.record.displayTitle(f.key.cleanTitle)).distinct.sorted
        // Count the new rows spawned beyond the remnant (a 1→N split = N−1).
        splitMetrics.recordSplit(fs.map(_.key).distinct.size - 1)
        SplitEvent(from.cleanTitle, into)
    }.toSeq

    val result = RebuildResult(changed, merges.toSeq, splits)
    logger.info(s"NormalizationRebuilder: changed=$changed, merges=${result.merges.size}, splits=${result.splits.size}")
    result
  }

  /** Re-resolve the rows whose external-API query (`apiQuery`) changed between
   *  `oldRules` and `newRules` — i.e. a Search-tier edit (programme prefix,
   *  accessibility tag, "+ event" suffix). Search rules don't affect the merge
   *  key, so `rebuild` leaves these rows alone; this re-enriches exactly the
   *  rows whose upstream query string moved, by republishing them. The
   *  enrichment query is `apiQuery(key.cleanTitle)` = `rules.search(cleanTitle)`,
   *  so comparing that per row targets precisely the affected set. */
  def reEnrichSearchChanges(oldRules: TitleRuleSet, newRules: TitleRuleSet,
                            publish: (String, Option[Int]) => Unit): Int = {
    var n = 0
    cache.entries.foreach { case (key, _) =>
      if (oldRules.search(key.cleanTitle) != newRules.search(key.cleanTitle)) {
        publish(key.cleanTitle, key.year); n += 1
      }
    }
    if (n > 0) logger.info(s"NormalizationRebuilder: re-enriched $n rows after a search-rule change.")
    n
  }
}

object NormalizationRebuilder {
  /** Rows that collapsed into one. */
  final case class MergeEvent(display: String, year: Option[Int], mergedTitles: Seq[String])
  /** A row that split into several. */
  final case class SplitEvent(from: String, into: Seq[String])
  final case class RebuildResult(changed: Int, merges: Seq[MergeEvent], splits: Seq[SplitEvent])
}

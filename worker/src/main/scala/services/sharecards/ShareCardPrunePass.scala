package services.sharecards

import java.nio.file.Path
import java.time.Instant
import scala.collection.mutable

/**
 * One [[ShareCardJanitor]] pass over the share-card directory as it was listed (`files`): what it deletes,
 * and why. A file is OLD once modified before `cutoff` (the grace period), and CURRENT while it carries a
 * live film's token or a catalogued city's slug; only an old file that is not current is ever deleted —
 * an old temp file is an abandoned write and goes too.
 */
private[sharecards] final class ShareCardPrunePass(
  store:      ShareCardStore,
  metrics:    ShareCardMetrics,
  files:      Seq[StoredFile],
  cutoff:     Instant,
  liveTokens: Set[String],
  cities:     Set[String]
) {
  import ShareCardMetrics.PruneReason

  private val deleted = mutable.Set.empty[Path]
  private val counts  = mutable.Map.empty[String, Int].withDefaultValue(0)

  private def old(file: StoredFile): Boolean = file.modified.isBefore(cutoff)
  private def current(file: StoredFile): Boolean = file.token.exists(liveTokens) || file.citySlug.exists(cities)
  private def retirable(file: StoredFile): Boolean = !file.temp && old(file) && !current(file)

  private def delete(file: StoredFile, reason: String): Unit =
    if (store.delete(file)) { deleted += file.path; counts(reason) += 1; metrics.pruned(file.kind, reason) }

  /** Whether this pass deleted the file at `path`. */
  def deletedPath(path: Path): Boolean = deleted(path)

  /** Each reason's deletions so far. */
  def deletions: Map[String, Int] = counts.toMap

  /** The files this pass has not deleted. */
  def kept: Seq[StoredFile] = files.filterNot(file => deleted(file.path))

  /** Abandoned writes: temp files older than the grace period. */
  def deleteAbandonedTemps(): Unit = files.filter(file => file.temp && old(file)).foreach(delete(_, PruneReason.Temp))

  /** A city gone from the catalog: its card goes at the next pass. Read from the code, not the read model,
   *  so no incomplete read can hold it back. */
  def deleteRetiredCities(): Unit =
    files.filter(file => file.citySlug.exists(slug => !cities(slug)) && old(file)).foreach(delete(_, PruneReason.Retired))

  /** Every old file no live film or city holds. */
  def deleteRetired(): Unit = files.filter(retirable).foreach(delete(_, PruneReason.Retired))

  /** The bytes of the files kept that are current, by kind. */
  def currentBytesByKind: Map[String, Long] = kept.filter(file => !file.temp && current(file)).groupMapReduce(_.kind)(_.bytes)(_ + _)

  /** Evict the oldest retirable files kept until the directory is under `budgetBytes`; returns the bytes left. */
  def evictOver(budgetBytes: Long): Long = {
    val remaining = kept
    var total = remaining.iterator.map(_.bytes).sum
    if (total > budgetBytes) {
      val it = remaining.filter(retirable).sortBy(_.modified).iterator
      while (total > budgetBytes && it.hasNext) {
        val file = it.next()
        delete(file, PruneReason.Budget)
        if (deleted(file.path)) total -= file.bytes
      }
    }
    total
  }
}

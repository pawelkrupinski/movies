package services.sharecards

import play.api.Logging
import services.readmodel.ReadModelReader

import java.time.Clock
import scala.concurrent.duration.*

/**
 * Keeps one country's share-card directory — each film's card, base and poster, under ONE budget —
 * in bounds. Run by the `PruneShareCards` task: daily in full ([[prune]]), and every few minutes for
 * the budget alone ([[enforceBudget]]). The prompt half of retirement is the projection's: a card
 * that leaves the read model has its files deleted at once ([[ShareCardService.onRetired]]); this is
 * the backstop for what that missed.
 *
 * WHAT IS NEVER DELETED, whoever runs it and whenever:
 *  - the files of a film a `web_movies` document points at a card of (in the daily prune: of a film
 *    on screen — a film that left the screens has its files retired, and its document re-projected
 *    so it stops pointing at them);
 *  - any file younger than the GRACE period: another replica may have just written it for a film
 *    it is about to publish;
 *  - a temp file younger than the grace period (a write in progress). Older ones are abandoned
 *    writes and go.
 *
 * CONCURRENT RUNS ARE SAFE BY CONSTRUCTION, not by a lock: every decision is read from the
 * directory and `web_movies` (shared by every replica), a delete of a vanished file is a no-op, and
 * the grace period covers every write in flight. A read of `web_movies` or `web_screenings` that
 * comes back incomplete deletes nothing but abandoned temps.
 */
class ShareCardJanitor(
  store:       ShareCardStore,
  reader:      ReadModelReader,
  budgetBytes: Long,
  metrics:     ShareCardMetrics,
  clock:       Clock,
  // Re-project a film whose card the prune retired while its document still pointed at it.
  refresh:     String => Unit,
  grace:       FiniteDuration = ShareCardJanitor.Grace
) extends Logging {
  import ShareCardMetrics.PruneReason

  def prune(): ShareCardJanitor.Report         = run(daily = true)
  def enforceBudget(): ShareCardJanitor.Report = run(daily = false)

  private def run(daily: Boolean): ShareCardJanitor.Report = {
    val cutoff               = clock.instant().minusMillis(grace.toMillis)
    val (refs, refsComplete) = reader.findAllShareCardRefsChecked()
    // Which films are on screen takes a scan of every screenings id — a country's largest
    // collection — so only the daily prune pays for it. The budget pass protects every film with a
    // card instead.
    val (screened, screensRead) =
      if (daily) {
        val (screenings, read) = reader.findAllScreeningRefsChecked()
        (screenings.iterator.map(_.filmId).toSet, read)
      } else (refs.iterator.filter(_.shareCard.nonEmpty).map(_.filmId).toSet, true)
    val complete   = refsComplete && screensRead
    val liveTokens = refs.iterator.filter(ref => screened(ref.filmId)).map(ref => ShareCardFile.token(ref.filmId)).toSet

    val files   = store.list()
    val deleted = scala.collection.mutable.Set.empty[java.nio.file.Path]
    val counts  = scala.collection.mutable.Map.empty[String, Int].withDefaultValue(0)
    def old(file: StoredFile): Boolean = file.modified.isBefore(cutoff)
    def current(file: StoredFile): Boolean = file.token.exists(liveTokens)
    def delete(file: StoredFile, reason: String): Unit =
      if (store.delete(file)) { deleted += file.path; counts(reason) += 1; metrics.pruned(file.kind, reason) }

    files.filter(file => file.temp && old(file)).foreach(delete(_, PruneReason.Temp))

    if (daily && complete) {
      files.filter(file => !file.temp && old(file) && !current(file)).foreach(delete(_, PruneReason.Retired))
      // A film off the screens whose document still points at a card just retired: re-project it,
      // so it points at nothing rather than at a missing file.
      refs.filter(ref => ref.shareCard.nonEmpty && !screened(ref.filmId) && deleted.contains(store.cardPath(ref.filmId)))
        .foreach(ref => refresh(ref.filmId))
    }

    val remaining    = files.filterNot(file => deleted(file.path))
    val currentBytes = remaining.filter(file => !file.temp && current(file)).groupMapReduce(_.kind)(_.bytes)(_ + _)
    var total = remaining.iterator.map(_.bytes).sum
    if (total > budgetBytes && complete) {
      val it = remaining.filter(file => !file.temp && old(file) && !current(file)).sortBy(_.modified).iterator
      while (total > budgetBytes && it.hasNext) {
        val file = it.next()
        delete(file, PruneReason.Budget)
        if (deleted(file.path)) total -= file.bytes
      }
    }
    val kept = files.filterNot(file => deleted(file.path))
    metrics.directory(
      bytesByKind   = kept.groupMapReduce(_.kind)(_.bytes)(_ + _),
      filesByKind   = kept.groupMapReduce(_.kind)(_ => 1L)(_ + _),
      currentByKind = currentBytes,
      budgetBytes   = budgetBytes)
    val report = ShareCardJanitor.Report(counts.toMap, kept.iterator.map(_.bytes).sum, currentBytes.values.sum, complete)
    if (report.currentBytes > budgetBytes)
      logger.error(s"share cards: the ${report.currentBytes} bytes of CURRENT cards, bases and posters alone exceed the " +
        s"$budgetBytes-byte budget — nothing more can be pruned; raise KINOWO_SHARE_CARD_BUDGET_MB.")
    if (!complete) logger.warn("share cards: web_movies/web_screenings read incomplete — pruned abandoned temp files only.")
    if (daily || counts.nonEmpty)
      logger.info(s"share cards ${if (daily) "prune" else "budget"}: deleted ${counts.toSeq.sorted.map { case (r, n) => s"$r=$n" }.mkString(", ")}; " +
        s"${report.bytes} bytes on disk (${report.currentBytes} current) of $budgetBytes budget.")
    report
  }
}

object ShareCardJanitor {
  /** An hour: longer than any render or poster fetch takes, and than a replica takes to record a
   *  card it wrote — so nothing still being written or about to be referenced is ever touched. */
  val Grace: FiniteDuration = 1.hour

  final case class Report(deleted: Map[String, Int], bytes: Long, currentBytes: Long, complete: Boolean)
}

package services.sharecards

import java.time.Clock

import scala.concurrent.duration.*

import play.api.Logging

/**
 * Keeps one country's share-card directory — each film's card, base and poster and each city's card, under ONE budget —
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
  // Which films' cards are live, read from the read model.
  liveCards:   LiveShareCards,
  budget:      settings.ShareCardStorageBudget,
  metrics:     ShareCardMetrics,
  clock:       Clock,
  // Re-project a film whose card the prune retired while its document still pointed at it.
  refresh:     String => Unit,
  grace:       FiniteDuration = ShareCardJanitor.Grace,
  // The country's city slugs: a city card is current while its city is in the catalog, and retired
  // (whichever pass finds it) once the city is not.
  cities:      Set[String] = Set.empty
) extends Logging {
  def prune(): ShareCardJanitor.Report         = run(daily = true)
  def enforceBudget(): ShareCardJanitor.Report = run(daily = false)

  private def run(daily: Boolean): ShareCardJanitor.Report = {
    val live = liveCards.read(daily)
    val pass = new ShareCardPrunePass(store, metrics, store.list(), clock.instant().minusMillis(grace.toMillis), live.tokens, cities)
    pass.deleteAbandonedTemps()
    pass.deleteRetiredCities()
    if (daily && live.complete) {
      pass.deleteRetired()
      // A film off the screens whose document still points at a card just retired: re-project it,
      // so it points at nothing rather than at a missing file.
      live.refs.filter(ref => ref.shareCard.nonEmpty && !live.screened(ref.filmId) && pass.deletedPath(store.cardPath(ref.filmId)))
        .foreach(ref => refreshEach(ref.filmId))
    }
    val currentBytes = pass.currentBytesByKind
    // What happens AT the limit: what the eviction could not bring under the budget is referenced,
    // so the directory takes no NEW file until a pass finds it back under ([[ShareCardRoom]]).
    if (live.complete) store.room.measured(overBudget = pass.evictOver(budget.bytes) > budget.bytes)
    val kept = pass.kept
    record(kept, currentBytes)
    val report = ShareCardJanitor.Report(pass.deletions, kept.iterator.map(_.bytes).sum, currentBytes.values.sum, live.complete)
    logReport(report, daily)
    report
  }

  // One film whose re-projection fails (its row unreadable for a moment) is logged and left for
  // the next prune; thrown on, it ended the loop, and every film after it kept pointing at a card
  // this run had just deleted, with the budget pass and the metrics skipped.
  private def refreshEach(filmId: String): Unit =
    try refresh(filmId)
    catch { case scala.util.control.NonFatal(e) => logger.warn(s"share cards: re-projecting $filmId after retiring its card failed: $e", e) }

  private def record(kept: Seq[StoredFile], currentBytes: Map[String, Long]): Unit = {
    metrics.volume(store.room.free(), ShareCardRoom.volumeSize(store.root), store.room.minimumFreeBytes)
    metrics.directory(
      bytesByKind   = kept.groupMapReduce(_.kind)(_.bytes)(_ + _),
      filesByKind   = kept.groupMapReduce(_.kind)(_ => 1L)(_ + _),
      currentByKind = currentBytes,
      budgetBytes   = budget.bytes)
  }

  private def logReport(report: ShareCardJanitor.Report, daily: Boolean): Unit = {
    if (report.currentBytes > budget.bytes)
      logger.error(s"share cards: the ${report.currentBytes} bytes of CURRENT cards, bases and posters alone exceed the " +
        s"${budget.bytes}-byte budget — nothing more can be pruned, and no new file is written; raise KINOWO_SHARE_CARD_BUDGET_MB.")
    if (!report.complete) logger.warn("share cards: web_movies/web_screenings read incomplete — pruned abandoned temp files only.")
    if (daily || report.deleted.nonEmpty)
      logger.info(s"share cards ${if (daily) "prune" else "budget"}: deleted ${report.deleted.toSeq.sorted.map { case (r, n) => s"$r=$n" }.mkString(", ")}; " +
        s"${report.bytes} bytes on disk (${report.currentBytes} current) of ${budget.bytes} budget.")
  }
}

object ShareCardJanitor {
  /** An hour: longer than any render or poster fetch takes, and than a replica takes to record a
   *  card it wrote — so nothing still being written or about to be referenced is ever touched. */
  val Grace: FiniteDuration = 1.hour

  final case class Report(deleted: Map[String, Int], bytes: Long, currentBytes: Long, complete: Boolean)

}

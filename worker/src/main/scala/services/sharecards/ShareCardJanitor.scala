package services.sharecards

import play.api.Logging
import services.readmodel.ReadModelReader

import java.time.Clock
import scala.concurrent.duration.*

/**
 * Keeps one country's share-card directory — cards, cached posters and card bases under ONE budget — in
 * bounds. Run by the `PruneShareCards` task: daily in full ([[prune]]), and every few minutes for
 * the budget alone ([[enforceBudget]]).
 *
 * WHAT IS NEVER DELETED, whoever runs it and whenever:
 *  - a card a `web_movies` document points at (the web may be serving it right now), the base it
 *    was drawn on, and a cached poster of a film on screen (in the budget pass, of a film with a
 *    card) — except that the daily prune retires the card of a film that has
 *    left the screens, and then re-projects its document so it stops pointing there;
 *  - any file younger than the GRACE period, whatever it looks like: another replica may have just
 *    written a card it hasn't recorded in `web_movies` yet, or be mid-render against a poster;
 *  - a temp file younger than the grace period (a write in progress). Older ones are abandoned
 *    writes and go.
 *
 * CONCURRENT RUNS ARE SAFE BY CONSTRUCTION, not by a lock: every decision is read from the
 * directory and `web_movies` (shared by every replica), a delete of a vanished file is a no-op, and
 * the grace period covers every write in flight. (The recurring enqueue is also claimed per window
 * through the scheduled-run store, so two replicas rarely run one at the same time anyway.) A read of
 * `web_movies` or `web_screenings` that comes back incomplete deletes nothing but abandoned temps.
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
  import ShareCardStore.Kind

  def prune(): ShareCardJanitor.Report         = run(daily = true)
  def enforceBudget(): ShareCardJanitor.Report = run(daily = false)

  private def run(daily: Boolean): ShareCardJanitor.Report = {
    val cutoff               = clock.instant().minusMillis(grace.toMillis)
    val (refs, refsComplete) = reader.findAllShareCardRefsChecked()
    // Which films are on screen takes a scan of every screenings id — a country's largest
    // collection — so only the daily prune pays for it. The budget pass, every ten minutes,
    // protects the posters of films that HAVE a card instead: a poster it evicts from a film
    // still waiting for one is fetched again, never lost.
    val (screened, screensRead) =
      if (daily) {
        val (screenings, read) = reader.findAllScreeningRefsChecked()
        (screenings.iterator.map(_.filmId).toSet, read)
      } else (refs.iterator.filter(_.shareCard.nonEmpty).map(_.filmId).toSet, true)
    val complete   = refsComplete && screensRead
    val live       = refs.filter(ref => screened(ref.filmId))
    val referenced = refs.iterator.flatMap(_.shareCard).toSet
    // A base is current while a card web_movies points at was drawn on it.
    val baseOf     = referenced.iterator.flatMap(ShareCardFile.parse).map(card => s"${card.baseKey}.jpg").toSet
    val onScreen   = live.iterator.flatMap(_.shareCard).toSet
    val posters    = live.iterator.flatMap(_.posterUrls).map(url => s"${ShareCardPosters.key(url)}.${ShareCardStore.PosterExtension}").toSet
    val tokens     = live.iterator.map(ref => ShareCardFile.token(ref.filmId)).toSet
    val cardOf     = live.iterator.flatMap(ref => ref.shareCard.map(ShareCardFile.token(ref.filmId) -> _)).toMap

    val files   = store.list()
    val deleted = scala.collection.mutable.Set.empty[java.nio.file.Path]
    val counts  = scala.collection.mutable.Map.empty[String, Int].withDefaultValue(0)
    def old(file: StoredFile): Boolean = file.modified.isBefore(cutoff)
    def delete(file: StoredFile, reason: String): Unit =
      if (store.delete(file)) { deleted += file.path; counts(reason) += 1; metrics.pruned(file.kind, reason) }

    files.filter(file => file.temp && old(file)).foreach(delete(_, PruneReason.Temp))

    if (daily && complete) {
      val cards = files.filter(file => file.kind == Kind.Card && !file.temp && old(file) && !onScreen(file.name))
      cards.foreach { file =>
        ShareCardFile.parse(file.name).foreach { card =>
          if (!tokens(card.token)) delete(file, PruneReason.Retired)
          else if (cardOf.get(card.token).exists(_ != file.name)) delete(file, PruneReason.Superseded)
        }
      }
      // A film off the screens whose document still points at a card just retired: re-project it,
      // so it points at nothing rather than at a missing file.
      refs.filterNot(ref => screened(ref.filmId))
        .filter(ref => ref.shareCard.exists(file => deleted.contains(store.cardPath(file))))
        .foreach(ref => refresh(ref.filmId))
      files.filter(file => file.kind == Kind.Poster && !file.temp && old(file) && !posters(file.name))
        .foreach(delete(_, PruneReason.Unreferenced))
      files.filter(file => file.kind == Kind.Base && !file.temp && old(file) && !baseOf(file.name))
        .foreach(delete(_, PruneReason.Unreferenced))
    }

    val remaining = files.filterNot(file => deleted(file.path))
    def current(file: StoredFile): Boolean = file.kind match {
      case Kind.Card   => referenced(file.name)
      case Kind.Poster => posters(file.name)
      case _           => baseOf(file.name)
    }
    val currentBytes = remaining.filter(file => !file.temp && current(file)).groupMapReduce(_.kind)(_.bytes)(_ + _)
    var total = remaining.iterator.map(_.bytes).sum
    if (total > budgetBytes && complete) {
      val evictable = remaining.filter(file => !file.temp && old(file) && !current(file)).sortBy(_.modified)
      val it = evictable.iterator
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
      logger.error(s"share cards: the ${report.currentBytes} bytes of CURRENT cards and posters alone exceed the " +
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

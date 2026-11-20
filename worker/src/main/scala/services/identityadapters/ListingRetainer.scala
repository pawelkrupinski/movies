package services.identityadapters

import java.util.concurrent.ConcurrentHashMap

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import play.api.Logging
import services.identity.*
import services.identity.measures.Listing
import services.titles.TitleNormalizer

/**
 * The model's kept listings ([[ListingRetention]], R1: a pure function of the inputs): every listing that left, with its
 * last-seen facts and when it left, persisted in `retainedStore`; and the departed ones the model holds as kept, recomputed
 * whole on every drain ([[retaining]]) and take-up ([[resumed]]) of [[IdentityModelService]], on the model's thread.
 */
final class ListingRetainer(retainedStore: RetainedListingStore, heldBefore: () => Seq[Listing], normalizer: TitleNormalizer,
                            clock: java.time.Clock, retentionWindow: java.time.Duration, metrics: IdentityModelMetrics)
    extends Logging {

  /** Reports the listings billed, kept and departed now ([[IdentityModelMetrics.retention]]). */
  def report(engine: IncrementalResolver): Unit =
    metrics.retention(engine.heldCount - retained.size, retained.size, departed.size)

  /** Every listing that left, with its last-seen facts and when it left ([[ListingRetention.Departed]]): an INPUT,
   *  persisted in `retainedStore` as it moves, and read back by a take-up ([[resumed]]). */
  private val departed = new ConcurrentHashMap[models.ListingKey, ListingRetention.Departed]()
  /** The departed listings the model holds as kept now: a cache of [[ListingRetention.retainedAt]] of the inputs, never
   *  a decision of its own -- each drain recomputes it whole and moves the model by the difference. */
  private val retained = java.util.concurrent.ConcurrentHashMap.newKeySet[models.ListingKey]()
  /** Each held listing's title as [[ListingRetention]] reads it -- by the one title relation ([[TitleRelation]]) -- by the
   *  listing object it was read off. */
  private val titles = new java.util.WeakHashMap[Listing, TitleRelation.Forms]()
  private def formsOf(listing: Listing): TitleRelation.Forms = titles.synchronized {
    Option(titles.get(listing)).getOrElse { val t = TitleRelation.of(listing, normalizer); titles.put(listing, t); t }
  }

  /** `seen` and the keys `leaving` as the model is handed them under [[ListingRetention]], a pure function of the inputs
   *  (R1, no stickiness): a listing back is billed again and is no departed one; a billed listing leaving is departed,
   *  with its facts as last held and the drain's time; a departed one past the retention window is no input. Then
   *  the listings kept are recomputed WHOLE from the departed and the billed ones, whatever order they came in -- one a
   *  related arrival bills again is kept again -- and the model moves by the difference from what it held as kept. */
  def retaining(engine: IncrementalResolver, seen: Seq[Listing],
                        leaving: Iterable[models.ListingKey]): (Seq[Listing], Seq[models.ListingKey]) = {
    val now      = clock.millis()
    val seenKeys = seen.iterator.map(_.key).toSet
    val back     = seenKeys.filter(departed.containsKey)
    back.foreach(departed.remove)
    val left     = departing(engine, seen, seenKeys, leaving, now)
    left.foreach { case (k, d) => departed.put(k, d) }
    val aged     = departed.asScala.collect { case (k, d) if !ListingRetention.live(d, now, retentionWindow) => k }.toSeq
    aged.foreach(departed.remove)
    val going    = leaving.toSet
    if (departed.isEmpty && retained.isEmpty) {
      persistDepartures(Map.empty, back, aged)
      (seen, going.toSeq)
    } else {
      val (newly, dropped) = rekept(engine, seen, seenKeys, going, now)
      persistDepartures(left, back, aged)
      (seen ++ newly, dropped)
    }
  }

  /** The departures written when any moved: those `left` now, those `back` and those `aged` past the window deleted. */
  private def persistDepartures(left: Map[models.ListingKey, ListingRetention.Departed], back: Set[models.ListingKey],
                                aged: Seq[models.ListingKey]): Unit =
    if (back.nonEmpty || left.nonEmpty || aged.nonEmpty) persist(retainedStore.update(left.values.toSeq, back ++ aged))

  /** The billed listings of `leaving` that depart now, each with its facts as last held and `now`. A listing held as kept
   *  that its venue's page no longer names left before: it keeps the departure it has. A departed listing its own venue
   *  bills again under another key (a lent page and year, another case) is back, not departed: one that leaves as its
   *  twin arrives is never written, one already stored is deleted -- that one alone, so the delete never reaches the
   *  archive's expired row of an earlier departure under a key leaving now. */
  private def departing(engine: IncrementalResolver, seen: Seq[Listing], seenKeys: Set[models.ListingKey],
                        leaving: Iterable[models.ListingKey], now: Long): Map[models.ListingKey, ListingRetention.Departed] = {
    val leftAll  = leaving.iterator.filterNot(k => retained.contains(k) || seenKeys(k)).flatMap(k => engine.held(k).map(k -> _))
      .map { case (k, listing) => k -> ListingRetention.Departed(listing, now) }.toMap
    val twins    = if (seen.isEmpty || (departed.isEmpty && leftAll.isEmpty)) Set.empty[models.ListingKey]
                   else ListingRetention.returnedUnderAnotherKey((departed.values.asScala ++ leftAll.values).map(_.listing).toSeq, seen)
                     .iterator.map(_.key).toSet
    val left     = leftAll -- twins
    val stored   = twins.filter(departed.containsKey)
    stored.foreach(departed.remove)
    if (stored.nonEmpty) deleteTwins(stored, atBoot = false)
    left
  }

  /** The listings kept recomputed WHOLE from the departed and the billed ones ([[ListingRetention.retainedAt]]), held as
   *  kept from now: those newly kept, and the keys the model lets go -- those `going` or kept before, kept no more and not
   *  billed. */
  private def rekept(engine: IncrementalResolver, seen: Seq[Listing], seenKeys: Set[models.ListingKey],
                     going: Set[models.ListingKey], now: Long): (Seq[Listing], Seq[models.ListingKey]) = {
    val present  = engine.listings.iterator.filterNot(l => going(l.key) || retained.contains(l.key) || seenKeys(l.key)).toSeq ++ seen
    val kept     = ListingRetention.retainedAt(departed.values.asScala.toSeq.sortBy(d => models.ListingKey.serialised(d.listing.key)),
      present, now, normalizer, formsOf, retentionWindow)
    val keptKeys = kept.iterator.map(_.key).toSet
    val newly    = kept.filterNot(l => retained.contains(l.key))
    val dropped  = (going ++ retained.asScala).filterNot(k => keptKeys(k) || seenKeys(k)).toSeq
    retained.clear(); keptKeys.foreach(retained.add)
    (newly, dropped)
  }

  /** The kept set a take-up resumes, against `present` (the listings billed now): [[ListingRetention.retainedAt]] of the
   *  departed listings the store holds -- the history's last held listings when it was never written. A departed listing
   *  persisted before departures were dated ([[ListingRetention.Unknown]]) is dated to this take-up and written back so
   *  (the store's migration: nothing it held is lost). Nothing billed read, or the store unread, resumes none and writes
   *  nothing: no read is a reason to forget an input. */
  def resumed(present: Seq[Listing]): Seq[Listing] = {
    retained.clear(); departed.clear()
    if (present.isEmpty) Nil
    else scala.util.Try(retainedStore.read()).fold(
      failed => { logger.warn(s"identity model: the departed listings could not be read -- none resumed: $failed"); Nil },
      read => resumedFrom(read, present))
  }

  /** [[resumed]] of what the store `read` (none: never written). */
  private def resumedFrom(read: Option[Seq[ListingRetention.Departed]], present: Seq[Listing]): Seq[Listing] = {
    val now     = clock.millis()
    val stored  = read.getOrElse(heldBefore().map(ListingRetention.Departed(_, ListingRetention.Unknown)))
    val dated   = stored.map(d => if (d.at == ListingRetention.Unknown) d.copy(at = now) else d)
    val billed  = present.iterator.map(_.key).toSet
    val (inputs, aged) = dated.filterNot(d => billed(d.listing.key)).partition(ListingRetention.live(_, now, retentionWindow))
    val twins = ListingRetention.returnedUnderAnotherKey(inputs.map(_.listing), present).iterator.map(_.key).toSet
    val live  = inputs.filterNot(d => twins(d.listing.key))
    if (twins.nonEmpty) deleteTwins(twins, atBoot = true)
    live.foreach(d => departed.put(d.listing.key, d))
    if (read.isEmpty || stored.exists(_.at == ListingRetention.Unknown) || aged.nonEmpty || live.size + twins.size != stored.size)
      persist(retainedStore.replace(live))
    val kept = ListingRetention.retainedAt(live, present, now, normalizer, formsOf, retentionWindow)
    kept.foreach(l => retained.add(l.key))
    if (stored.nonEmpty) logger.info(s"identity model: ${kept.size} kept listing(s) resumed of ${live.size} departed, ${aged.size} past ${retentionWindow.toDays} days")
    kept
  }

  /** `twins` (departed listings billed again under another key) deleted from the store, logged and counted: duplicates, so
   *  deleted, never expired. A delete that fails is logged; the next take-up or drain finds them again. */
  private def deleteTwins(twins: Set[models.ListingKey], atBoot: Boolean): Unit = {
    persist(retainedStore.delete(twins))
    metrics.twinsDeleted(twins.size, atBoot)
    logger.info(s"identity model: ${twins.size} departed listing(s) deleted as twins of a listing billed again under another key" +
      (if (atBoot) " (take-up)" else ""))
  }

  /** A write of the departed listings: one that fails is logged, never the drain's failure -- the next move writes again. */
  private def persist(write: => Unit): Unit =
    try write catch { case NonFatal(e) => logger.warn(s"identity model: the departed listings could not be written: $e") }
}

object ListingRetainer {
  /** What the kept listings are set up with besides the title pipeline, clock and gauges: where every departed listing
   *  is persisted (`store`), the listings the history last saw the model hold (`heldBefore`, [[RetainedListingStore.fromHistory]]:
   *  what a take-up reads the departed listings from when the store was never written), and how long a departed listing
   *  stays an input (`window`, [[ListingRetention.live]]: the worker's typed config `settings.IdentityRetentionWindow`). In
   *  memory, nothing held before, [[ListingRetention.DefaultMaxAge]], by default. */
  final case class Settings(store: RetainedListingStore = new RetainedListingStore.InMemory(),
                            heldBefore: () => Seq[Listing] = () => Nil,
                            window: java.time.Duration = ListingRetention.DefaultMaxAge)
  object Settings {
    /** The departed listings kept in memory, none held before, the default window: a fresh store each call. */
    def inMemory: Settings = Settings()
  }

  /** The kept listings `settings` set up, reading titles with `normalizer`, on `clock`, reporting to `metrics`. */
  def of(settings: Settings, normalizer: TitleNormalizer, clock: java.time.Clock, metrics: IdentityModelMetrics): ListingRetainer =
    new ListingRetainer(settings.store, settings.heldBefore, normalizer, clock, settings.window, metrics)
}

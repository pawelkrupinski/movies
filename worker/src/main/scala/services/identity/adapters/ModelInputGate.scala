package services.identity
package adapters

import play.api.Logging
import services.movies.ListingKey

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

/**
 * The model's input gate (docs/design/identity-structural-fixes.md, fix 1): a listing VERSION is taken into the model only
 * once each question its own facts lead to ([[ListingReadiness]]) was attempted; until then the model keeps the version it
 * holds (none, for a new one) and the questions are asked. The held listings are persisted in `store`, so a take-up resumes
 * them ([[resume]]); a listing held [[ModelInputGate.MaxHold]] is taken in regardless, and counted.
 *
 * One gate for the worker's model ([[IdentityModelService]]) and the drift replay's (`IdentityDrift.Worker`): the replay
 * differs only in the readiness it is handed (its own answers' attempts) and its clock, never in what is held or when.
 * Read and moved on the model's thread.
 */
final class ModelInputGate(
  readiness: ListingReadiness,
  store:     PendingListingStore,
  clock:     java.time.Clock,
  metrics:   IdentityModelMetrics = IdentityModelMetrics.Silent,
  maxHold:   java.time.Duration = ModelInputGate.MaxHold
) extends Logging {
  import ModelInputGate.Held

  /** The listings whose newest version the gate holds: an INPUT's cache -- the newest version is the venues', whether it is
   *  ready the answers', and what the model reads meanwhile `store`'s. */
  private val pending = new ConcurrentHashMap[ListingKey, Held]()
  /** The stand-ins of listings promoted since the last drain: let go by the next. */
  private val releasedStandIns = mutable.ArrayBuffer.empty[ListingKey]

  /** The held listings new to the model (no version of theirs taken in), by key. */
  def heldNew: Seq[Held] = pending.values.asScala.filter(_.ready.isEmpty).toSeq.sortBy(h => ListingKey.serialised(h.newest.key))
  /** Each held listing's stand-in (its version under the key its venue billed it by before): kept until it is taken in. */
  def standing: Set[ListingKey] = pending.values.asScala.iterator.flatMap(_.standIn).toSet
  /** The stand-ins of the listings promoted since the last call, the record of them cleared. */
  def takeReleasedStandIns(): Seq[ListingKey] = { val taken = releasedStandIns.toSeq; releasedStandIns.clear(); taken }

  /** The gate over a drain's scrapes: each listing billed otherwise than the model holds it -- new, or its facts moved -- is
   *  taken in only once its own questions were attempted; until then the model keeps the version it holds and the
   *  questions are asked. A listing billed as the model holds it, or no longer billed, is held no more. Then every held
   *  listing whose answers came is promoted. Returns the scraped listings the model takes in, and the promoted ones. */
  def admit(engine: IncrementalResolver, scraped: Seq[(String, Seq[Listing])]): (Seq[Listing], Seq[Listing]) = {
    val now      = clock.millis()
    val billed   = scraped.iterator.flatMap(_._2.iterator.map(_.key)).toSet
    val gone     = scraped.flatMap { case (venue, listings) =>
      val keys = listings.iterator.map(_.key).toSet
      pending.values.asScala.collect { case h if h.venue == venue && !keys(h.newest.key) => h.newest.key } }
    val written  = mutable.ArrayBuffer.empty[PendingListingStore.Pending]
    val released = mutable.ArrayBuffer.empty[ListingKey] ++= gone
    gone.foreach(pending.remove)
    val taken = scraped.flatMap { case (venue, listings) => listings.filter { listing =>
      val prior = engine.held(listing.key).orElse(standInFor(engine, venue, listing, billed))
      if (prior.contains(listing)) { if (pending.remove(listing.key) != null) released += listing.key; true }
      else {
        val waits = readiness.awaiting(listing)
        if (waits.isEmpty) { if (pending.remove(listing.key) != null) released += listing.key; true }
        else {
          val was  = Option(pending.get(listing.key))
          val held = Held(venue, listing, was.flatMap(_.ready).orElse(prior), waits, was.fold(now)(_.since))
          pending.put(listing.key, held)
          if (was.exists(_.newest == listing)) askLedTo(was.get, waits)
          else {
            readiness.ask(listing, waits)
            logger.info(s"identity model: held ${ListingKey.serialised(listing.key)} on ${waits.size} question(s) of its own " +
              s"(${waits.toSeq.flatMap(_.id).sorted.take(3).mkString(", ")}); the model ${if (prior.isDefined) "keeps the version it holds" else "holds no version of it"}")
          }
          if (!was.exists(w => w.ready == held.ready)) written += PendingListingStore.Pending(listing.key, held.ready, held.since)
          false
        }
      }
    } }
    val promoted = promote(billed)
    released ++= promoted.map(_.key)
    if (written.nonEmpty || released.nonEmpty) persist(written.toSeq, released.toSeq)
    report()
    (taken, promoted)
  }

  /** The billed listings `present` as the gate lets the model take them up: each the store holds pending is read afresh --
   *  ready now (its answers filed while the worker was down), its newest version taken in and its row released; else held
   *  again, its questions asked again, the model reading its last ready version (none, for a new one) -- and a row whose
   *  listing is billed no more released. Idempotent: whether a version is ready is never stored, so a take-up after one
   *  that stopped mid-way reads the same. An unread store holds nothing back: no read is a reason to keep a version out. */
  def resume(present: Seq[Listing]): Seq[Listing] = {
    pending.clear(); releasedStandIns.clear()
    val rows = scala.util.Try(store.read()).fold(
      failed => { logger.warn(s"identity model: the held listings could not be read -- none held: $failed"); Nil }, identity)
    val taken =
      if (rows.isEmpty) present
      else {
        val byKey    = rows.iterator.map(r => r.key -> r).toMap
        val billed   = present.iterator.map(_.key).toSet
        val released = mutable.ArrayBuffer.empty[ListingKey] ++= rows.iterator.map(_.key).filterNot(billed)
        val bootPromoted = mutable.ArrayBuffer.empty[Double]
        val resumed = present.flatMap { listing => byKey.get(listing.key) match {
          case None => Some(listing)
          case Some(row) =>
            val overdue = clock.millis() - row.since >= maxHold.toMillis
            val waits   = if (overdue) Set.empty[Question] else readiness.awaiting(listing)
            val held    = (clock.millis() - row.since).max(0L) / 1000.0
            if (overdue) metrics.bounded(Seq(held))
            if (waits.isEmpty) { released += listing.key; if (!overdue) bootPromoted += held; Some(listing) }
            else {
              pending.put(listing.key, Held(listing.venue, listing, row.ready, waits, row.since))
              readiness.ask(listing, waits)
              row.ready
            }
        } }
        if (released.nonEmpty) persist(Nil, released.toSeq)
        if (bootPromoted.nonEmpty) metrics.promoted(bootPromoted.toSeq, atBoot = true)
        logger.info(s"identity model: ${pending.size} listing(s) held on their questions resumed, ${released.size} released" +
          pending.values.asScala.toSeq.sortBy(_.since).take(20).map(h => s"; ${ListingKey.serialised(h.newest.key)} " +
            s"held ${(clock.millis() - h.since) / 60000} min on ${h.waits.toSeq.flatMap(_.id).sorted.take(3).mkString(", ")}" +
            (if (h.ready.isEmpty) " (no version taken in)" else "")).mkString)
        resumed
      }
    report()
    taken
  }

  /** The version the model holds of `listing` under ANOTHER key at its venue -- the same clean title, a key the scrape no
   *  longer bills: the venue re-keyed it (a page the scrape dropped turned its Native key Published, DE Mal seh'n Kino's "IF
   *  PIGEONS TURNED TO GOLD", 2026-11-06). Read as the version the model holds, so the gate holds the re-keyed listing as a
   *  changed one -- its old version served meanwhile -- never as one new to the model, whose film the site lost. */
  private def standInFor(engine: IncrementalResolver, venue: String, listing: Listing, billed: ListingKey => Boolean): Option[Listing] =
    engine.heldAt(venue).iterator.filterNot(billed).flatMap(engine.held).filter(_.cleanTitle.equalsIgnoreCase(listing.cleanTitle))
      .toSeq.sortBy(l => ListingKey.serialised(l.key)).headOption

  /** The held listings, but those `skip` names, whose questions were all attempted: taken in now -- how long each waited
   *  told to the metrics. */
  private def promote(skip: ListingKey => Boolean): Seq[Listing] = {
    val promotedAfter = mutable.ArrayBuffer.empty[Double]
    val bounded       = mutable.ArrayBuffer.empty[Double]
    val taken = pending.values.asScala.toSeq.filterNot(h => skip(h.newest.key)).sortBy(h => ListingKey.serialised(h.newest.key))
      .flatMap { h =>
        val late  = overdue(h)
        val waits = if (late) Set.empty[Question] else readiness.awaiting(h.newest)
        val held  = (clock.millis() - h.since) / 1000.0
        if (waits.isEmpty) {
          pending.remove(h.newest.key); (if (late) bounded else promotedAfter) += held; releasedStandIns ++= h.standIn; Some(h.newest)
        }
        else { if (waits != h.waits) { askLedTo(h, waits); pending.put(h.newest.key, h.copy(waits = waits)) }; None }
      }
    if (promotedAfter.nonEmpty) metrics.promoted(promotedAfter.toSeq, atBoot = false)
    if (bounded.nonEmpty) { metrics.bounded(bounded.toSeq); logger.warn(s"identity model: ${bounded.size} held listing(s) taken in past the ${maxHold.toMinutes} min bound") }
    taken
  }
  /** The questions `held`'s listing waits on now that it did not when last asked -- those its answers led to (a title's
   *  search names films whose records it then waits on) -- asked ahead as its first ones were. Left to the agreement's
   *  queue, they were answered only after the bound had taken the listing in (DE, US, image 5ec9cf7, 2026-11-06). */
  private def askLedTo(held: Held, waits: Set[Question]): Unit = {
    val led = waits -- held.waits
    if (led.nonEmpty) readiness.ask(held.newest, led)
  }
  /** Held past [[maxHold]], by the injected clock: taken in whatever it still waits on. */
  private def overdue(held: Held): Boolean = clock.millis() - held.since >= maxHold.toMillis

  /** How many listings the gate holds and how long the oldest has waited, by the injected clock. */
  def report(): Unit = {
    val now    = clock.millis()
    val oldest = pending.values.asScala.iterator.map(_.since).minOption
    metrics.held(pending.size, oldest.fold(0.0)(since => (now - since).max(0L) / 1000.0))
  }

  /** A write of the held listings: one that fails is logged, never the drain's failure -- the next move writes again, and a
   *  take-up reads readiness afresh. */
  private def persist(written: Seq[PendingListingStore.Pending], released: Seq[ListingKey]): Unit =
    try store.update(written, released)
    catch { case NonFatal(e) => logger.warn(s"identity model: the held listings could not be written: $e") }
}

object ModelInputGate {
  /** How long the input gate holds a listing at most: its questions are asked ahead of the queue and wait one attempt each,
   *  so a hold past this is a question the queue does not reach -- taken in, counted, and paged on by the alert's count. */
  val MaxHold: java.time.Duration = java.time.Duration.ofMinutes(10)

  /** A listing whose newest version (`newest`, billed at `venue`) the gate holds on `waits` since `since`; the model reads
   *  `ready` meanwhile -- none for a listing new to it. */
  final case class Held(venue: String, newest: Listing, ready: Option[Listing], waits: Set[Question], since: Long) {
    /** The key the model reads meanwhile when it is another listing's: the one this listing was billed under before its
     *  venue re-keyed it (a page dropped, a title re-cased) -- kept in the model until this one is taken in. */
    def standIn: Option[ListingKey] = ready.map(_.key).filterNot(_ == newest.key)
  }
}

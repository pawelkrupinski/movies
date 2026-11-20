package services.identityadapters

import services.identity.*
import services.identity.measures.Listing
import play.api.Logging
import models.ListingKey

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal
import services.identity.vocabulary.Question

/**
 * The model's input gate (docs/design/identity-structural-fixes.md, fix 1): a listing VERSION is taken into the model only
 * once each question its own facts lead to ([[ListingReadiness]]) was attempted -- one a broken host turned away
 * included, by design, but a poster only once hashed, known gone or given up on ([[AgreementReadiness]],
 * [[QuestionMemory.admits]]: a failed one retried meanwhile); until then the model keeps the version it
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
  /** The keys whose last write to `store` failed -- a hold's row or a release -- written again by the next [[persist]],
   *  from what the gate holds then. A row is otherwise written only as its hold moves, so one lost write was lost for good
   *  and a restart took the unready version in. */
  private val unwritten = mutable.Set.empty[ListingKey]

  /** Every held listing's newest version: what the gate's next pass reads the readiness of. */
  def heldVersions: Seq[Listing] = pending.values.asScala.iterator.map(_.newest).toSeq
  /** The held listings new to the model (no version of theirs taken in), by key. */
  def heldNew: Seq[Held] = pending.values.asScala.filter(_.ready.isEmpty).toSeq.sortBy(h => ListingKey.serialised(h.newest.key))
  /** Each held listing's stand-in (its version under the key its venue billed it by before): kept until it is taken in. */
  def standing: Set[ListingKey] = pending.values.asScala.iterator.flatMap(_.standIn).toSet
  /** The stand-ins of the listings promoted since the last call, the record of them cleared. */
  def takeReleasedStandIns(): Seq[ListingKey] = { val taken = releasedStandIns.toSeq; releasedStandIns.clear(); taken }

  /** The gate over a drain's scrapes: each listing billed otherwise than the model holds it -- new, its facts moved, or only
   *  what the stages after the model read (a poster, names, words, days: [[ModelInputGate.billedAlike]]) -- is
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
    val taken = scraped.flatMap { case (venue, listings) => listings.filter(takenIn(engine, venue, _, billed, now, written, released)) }
    val promoted = promote(billed)
    released ++= promoted.map(_.key)
    persist(written.toSeq, released.toSeq)
    report()
    (taken, promoted)
  }

  /** Is `listing`, scraped at `venue`, taken into the model now: billed as the model holds it (or the version it holds
   *  under another key, [[standInFor]]), or none of its own questions open -- its hold, if any, `released`? Else it is
   *  held ([[hold]]). */
  private def takenIn(engine: IncrementalResolver, venue: String, listing: Listing, billed: ListingKey => Boolean, now: Long,
                      written: mutable.ArrayBuffer[PendingListingStore.Pending], released: mutable.ArrayBuffer[ListingKey]): Boolean = {
    val prior = engine.held(listing.key).orElse(standInFor(engine, venue, listing, billed))
    val waits = if (prior.exists(ModelInputGate.billedAlike(_, listing))) Set.empty[Question] else readiness.awaiting(listing)
    if (waits.isEmpty) { if (pending.remove(listing.key) != null) released += listing.key; true }
    else { hold(venue, listing, prior, waits, now, written); false }
  }

  /** `listing` held on `waits`, the model reading `prior` (or the version it was held over before) meanwhile: its
   *  questions asked -- only those its answers led to, when held so already -- and its row `written` when what the model
   *  reads moved. */
  private def hold(venue: String, listing: Listing, prior: Option[Listing], waits: Set[Question], now: Long,
                   written: mutable.ArrayBuffer[PendingListingStore.Pending]): Unit = {
    val was  = Option(pending.get(listing.key))
    val held = Held(venue, listing, was.flatMap(_.ready).orElse(prior), waits, was.fold(now)(_.since))
    pending.put(listing.key, held)
    if (was.exists(w => ModelInputGate.billedAlike(w.newest, listing))) askLedTo(was.get, waits)
    else {
      readiness.ask(listing, waits)
      logger.info(s"identity model: held ${ModelInputGate.described(listing.key)} on ${waits.size} question(s) of its own " +
        s"(${waits.toSeq.flatMap(_.id).sorted.take(3).mkString(", ")}); the model ${if (prior.isDefined) "keeps the version it holds" else "holds no version of it"}")
    }
    if (!was.exists(w => w.ready == held.ready)) written += PendingListingStore.Pending(listing.key, held.ready, held.since)
  }

  /** The billed listings `present` as the gate lets the model take them up: each the store holds pending is read afresh --
   *  ready now (its answers filed while the worker was down), its newest version taken in and its row released; else held
   *  again, its questions asked again, the model reading its last ready version (none, for a new one) -- and a row whose
   *  listing is billed no more released. Idempotent: whether a version is ready is never stored, so a take-up after one
   *  that stopped mid-way reads the same. An unread store holds nothing back: no read is a reason to keep a version out. */
  def resume(present: Seq[Listing]): Seq[Listing] = {
    pending.clear(); releasedStandIns.clear(); unwritten.clear()
    val rows = scala.util.Try(store.read()).fold(
      failed => { logger.warn(s"identity model: the held listings could not be read -- none held: $failed"); Nil }, identity)
    val taken =
      if (rows.isEmpty) present
      else {
        val byKey    = rows.iterator.map(r => r.key -> r).toMap
        val billed   = present.iterator.map(_.key).toSet
        val released = mutable.ArrayBuffer.empty[ListingKey] ++= rows.iterator.map(_.key).filterNot(billed)
        val bootPromoted = mutable.ArrayBuffer.empty[Double]
        val resumed = present.flatMap(listing => byKey.get(listing.key).fold(Option(listing))(resumedOf(listing, _, released, bootPromoted)))
        persist(Nil, released.toSeq)
        if (bootPromoted.nonEmpty) metrics.promoted(bootPromoted.toSeq, atBoot = true)
        logResumed(released.size)
        resumed
      }
    report()
    taken
  }

  /** `listing`, held pending in `row` when the worker stopped, as the take-up reads it: taken in when ready or held past
   *  the bound (its row `released`, how long it waited counted), else held again with its questions asked, the model
   *  reading its last ready version. */
  private def resumedOf(listing: Listing, row: PendingListingStore.Pending, released: mutable.ArrayBuffer[ListingKey],
                        bootPromoted: mutable.ArrayBuffer[Double]): Option[Listing] = {
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
  }

  /** The take-up's line: the listings held again on their questions, the oldest twenty named, and how many released. */
  private def logResumed(released: Int): Unit =
    logger.info(s"identity model: ${pending.size} listing(s) held on their questions resumed, $released released" +
      pending.values.asScala.toSeq.sortBy(_.since).take(20).map(h => s"; ${ModelInputGate.described(h.newest.key)} " +
        s"held ${(clock.millis() - h.since) / 60000} min on ${h.waits.toSeq.flatMap(_.id).sorted.take(3).mkString(", ")}" +
        (if (h.ready.isEmpty) " (no version taken in)" else "")).mkString)

  /** The version the model holds of `listing` under ANOTHER key at its venue -- the same clean title, a key the scrape no
   *  longer bills: the venue re-keyed it (a page the scrape dropped turned its Native key Published, DE Mal seh'n Kino's "IF
   *  PIGEONS TURNED TO GOLD", 2026-11-06). Read as the version the model holds, so the gate holds the re-keyed listing as a
   *  changed one -- its old version served meanwhile -- never as one new to the model, whose film the site lost. */
  private def standInFor(engine: IncrementalResolver, venue: String, listing: Listing, billed: ListingKey => Boolean): Option[Listing] =
    engine.heldAt(venue).iterator.filterNot(billed).flatMap(engine.held).filter(_.cleanTitle.equalsIgnoreCase(listing.cleanTitle))
      .toSeq.sortBy(l => ListingKey.serialised(l.key)).headOption

  /** The held listings, but those `skip` names, whose questions were all attempted: taken in now -- how long each waited
   *  told to the metrics. One still waiting has its failed questions asked again once their backoff ended
   *  ([[ListingReadiness.retry]]). */
  private def promote(skip: ListingKey => Boolean): Seq[Listing] = {
    val taken = pending.values.asScala.toSeq.filterNot(h => skip(h.newest.key)).sortBy(h => ListingKey.serialised(h.newest.key))
      .flatMap(promoted)
    val (bounded, inTime) = taken.partition(_.late)
    if (inTime.nonEmpty) metrics.promoted(inTime.map(_.heldSeconds), atBoot = false)
    if (bounded.nonEmpty) { metrics.bounded(bounded.map(_.heldSeconds)); logger.warn(s"identity model: ${bounded.size} held listing(s) taken in past the ${maxHold.toMinutes} min bound") }
    taken.map(_.listing)
  }
  /** `held` taken in when its questions were all attempted or it is [[overdue]] -- its stand-in released -- else
   *  [[stillHeld]]. */
  private def promoted(held: Held): Option[ModelInputGate.Promoted] = {
    val late  = overdue(held)
    val waits = if (late) Set.empty[Question] else readiness.awaiting(held.newest)
    val heldSeconds = (clock.millis() - held.since) / 1000.0
    if (waits.nonEmpty) { stillHeld(held, waits); None }
    else {
      pending.remove(held.newest.key); releasedStandIns ++= held.standIn
      Some(ModelInputGate.Promoted(held.newest, late, heldSeconds))
    }
  }
  /** `held`, still waiting on `waits`: the questions its answers led to asked ([[askLedTo]]) and its hold moved to them,
   *  and its failed questions asked again -- a failed poster holds the listing (QuestionMemory.admits): asked again here
   *  once its backoff ends. */
  private def stillHeld(held: Held, waits: Set[Question]): Unit = {
    if (waits != held.waits) { askLedTo(held, waits); pending.put(held.newest.key, held.copy(waits = waits)) }
    readiness.retry(held.newest, waits)
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

  /** A write of the held listings, with every key whose last write failed written again as the gate holds it now (its row,
   *  or its release): one that fails is logged, never the drain's failure, and its keys are written again by the next. */
  private def persist(written: Seq[PendingListingStore.Pending], released: Seq[ListingKey]): Unit = {
    val fresh   = written.iterator.map(_.key).toSet ++ released
    val retried = unwritten.iterator.filterNot(fresh).toSeq
    val rows    = written ++ retried.flatMap(key => Option(pending.get(key)).map(h => PendingListingStore.Pending(key, h.ready, h.since)))
    val gone    = released ++ retried.filterNot(pending.containsKey)
    if (rows.nonEmpty || gone.nonEmpty)
      try { store.update(rows, gone); unwritten.clear() }
      catch { case NonFatal(e) =>
        unwritten ++= rows.iterator.map(_.key) ++= gone
        logger.warn(s"identity model: the held listings could not be written -- written again at the next drain: $e")
      }
  }
}

object ModelInputGate {
  /** What a gate is set up with besides its clock and gauges: whether a listing's questions are answered (`readiness`),
   *  where the held listings are persisted (`store`), and the backstop (`maxHold`). Always ready, in memory, by default. */
  final case class Settings(readiness: ListingReadiness = ListingReadiness.Always,
                            store: PendingListingStore = new PendingListingStore.InMemory(),
                            maxHold: java.time.Duration = MaxHold)
  object Settings {
    /** Every listing ready at once, the held ones kept in memory, the backstop the default one: a fresh store each call. */
    def inMemory: Settings = Settings()
  }

  /** The gate `settings` set up, on `clock`, reporting to `metrics`. */
  def of(settings: Settings, clock: java.time.Clock, metrics: IdentityModelMetrics): ModelInputGate =
    new ModelInputGate(settings.readiness, settings.store, clock, metrics, settings.maxHold)

  /** A listing's key as a log line names it: its venue and raw title, never the serialised key, whose NUL separators
   *  turned the drift replay's log binary to grep (the drift check read no verdict, run 42230476293). */
  def described(key: ListingKey): String = s"${key.venue} | ${key.rawTitle}"

  /** Is `now` billed exactly as the model holds `was`: equal, and alike in what [[Listing]]'s equality leaves out and the
   *  stages after the model read ([[Listing.movedOutsideEquality]]: its poster, names, words, screening days)? Only then is
   *  it let through unasked. Equality alone let a listing re-published with a new poster straight in: the agreement read
   *  the unhashed poster off the model's listing, waited on it, and the cluster was served the model's "no film" until its
   *  filing restored the take (prod 2026-11-11, image 4061cc3: UK Odeon "Disco" x5 for 3 s, "Touché" x5 for 10 s, as
   *  Odeon's posters first arrived). Held instead, the version the model holds -- and its take -- stands until the new
   *  one's questions are answered (the owner: "they should stay the same until all is resolved for the new one"). */
  def billedAlike(was: Listing, now: Listing): Boolean = was == now && !Listing.movedOutsideEquality(was, now)

  /** How long the input gate holds a listing at most: its questions are asked ahead of the queue and wait one attempt each,
   *  so a hold past this is a question the queue does not reach -- taken in, counted, and paged on by the alert's count.
   *  The one bound of a take held on its answers: the agreement stage's previous take standing while a refreshed answer's
   *  questions are checked has it too ([[agreement.AgreementStage.HoldAtMost]]). */
  val MaxHold: java.time.Duration = agreement.AgreementStage.HoldAtMost

  /** A held listing taken in: its newest version, whether past the bound (`late`), and how long it was held. */
  private final case class Promoted(listing: Listing, late: Boolean, heldSeconds: Double)

  /** A listing whose newest version (`newest`, billed at `venue`) the gate holds on `waits` since `since`; the model reads
   *  `ready` meanwhile -- none for a listing new to it. */
  final case class Held(venue: String, newest: Listing, ready: Option[Listing], waits: Set[Question], since: Long) {
    /** The key the model reads meanwhile when it is another listing's: the one this listing was billed under before its
     *  venue re-keyed it (a page dropped, a title re-cased) -- kept in the model until this one is taken in. */
    def standIn: Option[ListingKey] = ready.map(_.key).filterNot(_ == newest.key)
  }
}

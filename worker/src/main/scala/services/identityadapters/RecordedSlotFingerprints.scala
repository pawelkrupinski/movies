package services.identityadapters

import scala.util.{Failure, Success, Try}

import services.identity.{VenueSlotFingerprints, VenueSlotMemo}

/** The venue slot fingerprints the store holds (`identity_slot_fingerprints`), as far as this worker knows: read once
 *  to [[seed]] the projection's slot memo after a boot, so a restarted worker reuses the slots the last run kept, and
 *  kept in step as each periodic projection [[record]]s what the memo then keeps. Logs under the projection's `logger`. */
final class RecordedSlotFingerprints(fingerprints: VenueSlotFingerprints, logger: play.api.Logger) {
  /** The fingerprints the store holds, as far as this worker knows: none read yet until the first projection. */
  private var recorded: Option[Set[Long]] = None

  /** Seed `slotMemo`, before the first projection after a boot, with the slots the last run kept. */
  def seed(slotMemo: VenueSlotMemo): Unit = if (recorded.isEmpty) {
    val stored = Try(fingerprints.all()).fold(e => { logger.warn("identity projection: reading the venue slot fingerprints failed; " +
      "the first projection builds every slot", e); Set.empty[Long] }, identity)
    slotMemo.seed(stored)
    recorded = Some(stored)
  }

  /** Record the fingerprints of the slots `slotMemo` now keeps, writing only those that moved. One that fails is
   *  tried again by the next projection; one left behind is a true fact, costing only its space. */
  def record(slotMemo: VenueSlotMemo): Unit = {
    val known = recorded.getOrElse(Set.empty)
    val now   = slotMemo.fingerprints
    if (now != known) Try(fingerprints.update(now -- known, known -- now)) match {
      case Success(_) => recorded = Some(now)
      case Failure(e) => logger.warn("identity projection: recording the venue slot fingerprints failed; the next projection retries", e)
    }
  }
}

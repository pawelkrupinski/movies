package services.identity
package adapters

import scala.util.{Failure, Success, Try}

/** The venue slots the identity projection's memo keeps, recorded by their fingerprints (`identity_slot_fingerprints`)
 *  so a restarted worker reuses them ([[IdentityProjection]]). Logs under the projection's `logger`. */
private[identity] final class SlotFingerprintRecord(fingerprints: VenueSlotFingerprints, slotMemo: VenueSlotMemo,
                                                    logger: play.api.Logger) {

  /** The fingerprints the store holds, as far as this worker knows: none read yet until the first projection. */
  private var recordedFingerprints: Option[Set[Long]] = None

  /** Seed the memo, before the first projection after a boot, with the slots the last run kept. */
  def seed(): Unit = if (recordedFingerprints.isEmpty) {
    val recorded = Try(fingerprints.all()).fold(e => { logger.warn("identity projection: reading the venue slot fingerprints failed; " +
      "the first projection builds every slot", e); Set.empty[Long] }, identity)
    slotMemo.seed(recorded)
    recordedFingerprints = Some(recorded)
  }

  /** Record the fingerprints of the slots the memo now keeps, writing only those that moved. One that fails is
   *  tried again by the next projection; one left behind is a true fact, costing only its space. */
  def record(): Unit = {
    val known = recordedFingerprints.getOrElse(Set.empty)
    val now   = slotMemo.fingerprints
    if (now != known) Try(fingerprints.update(now -- known, known -- now)) match {
      case Success(_) => recordedFingerprints = Some(now)
      case Failure(e) => logger.warn("identity projection: recording the venue slot fingerprints failed; the next projection retries", e)
    }
  }
}

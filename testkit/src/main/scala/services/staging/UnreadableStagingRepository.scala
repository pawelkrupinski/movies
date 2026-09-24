package services.staging

import models.{MovieRecord, Source}
import tools.contracts.FailsOnPurpose

/** A [[StagingRepository]] whose whole-collection scan is INCOMPLETE while `failing` — the
 *  shape `MongoStagingRepository.findAll` gives when its keyset scan runs out of retries:
 *  no rows, flagged incomplete. Writes and the per-film reads stay real, so a spec can seed
 *  staging and then blind only the scan. */
class UnreadableStagingRepository(seed: Seq[(Source, String, Option[Int], MovieRecord)] = Seq.empty)
  extends InMemoryStagingRepository(seed) with FailsOnPurpose {
  @volatile var failing: Boolean = true
  override def findAll(): Seq[StagingRecord] = if (failing) Seq.empty else super.findAll()
  override def findAllChecked(): (Seq[StagingRecord], Boolean) =
    if (failing) (Seq.empty, false) else super.findAllChecked()
}

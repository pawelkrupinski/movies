package services.identityadapters

import services.identity.*
import services.identity.staging.StagingEvent

/** What a staging-screen snapshot ([[StagingSnapshots]]) reads its takes against. */
private[services] object StagingReadings {
  /** What a publish reads its takes against: each listing's facts as a change carried them, else as the projection last
   *  read them, walked only when asked. */
  def of(resolution: Resolution, changed: Seq[StagingEvent.Changed], storedFilm: Int => Option[IdentityMeasures.Film],
         read: () => Iterator[Listing]): StagingReading = {
    val carried = changed.iterator.flatMap(c => c.change.published.map(c.change.listing -> _)).toMap
    lazy val lastRead = read().map(l => l.key -> l).toMap
    new StagingReading(resolution, storedFilm, key => carried.get(key).orElse(lastRead.get(key)).fold(Nil)(StagedEvidence.listing))
  }
}

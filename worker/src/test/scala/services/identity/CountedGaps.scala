package services.identity


import services.identity.adapters.TmdbIdentityLookups
import scala.util.Try

/** A replay counting the requests it could not answer in one counter (`misses`): a lookup during
 *  which it grew met a gap. The counter is the replay's, not the lookup's, so lookups take turns. */
final class CountedGaps(misses: () => Long) extends TmdbIdentityLookups.Gaps {
  def answered[A](read: => A): Answer[A] = synchronized {
    val before = misses()
    Try(read).toOption.filter(_ => misses() == before).toAnswer
  }
}

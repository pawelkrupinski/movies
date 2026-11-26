package services.identityprojection

import services.identity.IdentityLookups
import services.identity.measures.Film

/** TMDB's records of a set of films as a caller's own lookups hold them: what [[SuspectTakeCount]] reads its takes'
 *  films by. */
object PrefetchedRecords {
  /** The records `lookups` holds of the films asked for, read in ONE prefetch rather than a read per film, and let go
   *  once read: `lookups` must be the caller's own, as a prefetch drops whatever the last one held. */
  def of(lookups: IdentityLookups): Seq[Int] => Map[Int, Film] = films => {
    lookups.prefetch(Nil, films, Nil)
    try films.flatMap(id => lookups.film(id).answered.flatten.map(id -> _)).toMap
    finally lookups.prefetchAnswered()
  }
}

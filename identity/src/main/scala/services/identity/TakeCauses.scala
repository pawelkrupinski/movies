package services.identity

import services.movies.ListingKey

/** Why each take of a projection's change list moved ([[TakeChange.Cause]]): read off what the take stood on, never off when
 *  it moved. The stored film a listing was on before the writes ([[TakeChange.previousFilm]], as the projection read it)
 *  is the take's receipt across builds: its venue slots name every listing the take rested on, as written. A take is
 *  EVIDENCE when
 *  - a listing of those slots is published no more: neither under its key nor at its slot (PL "Kupiłem motocykl, który
 *    jest wampirem", 2026-10-24 18:51Z: eleven listings rested on Kino Pionier 1907's year and director, and lost the
 *    film when its one screening passed and the scrape dropped it), or
 *  - one of them, or a listing of the decision taking it now, was re-published otherwise by this update (its facts,
 *    poster, days, names or words: [[LiveProjectionIndex.lastRepublished]]; not known of an index built afresh), or
 *  - the projection was set off by an answer filed or a stored film another writer moved (an answer withdrawn, a TMDB
 *    record read again) — once the store holds this build's own takes.
 *
 *  With none of those, the inputs read the same and the decision moved: CODE, while the store holds another build's takes
 *  (`ownTakes` false: until a projection of this process wrote all it planned, the first of the whole corpus), or when the
 *  decision this build made was replaced ([[TakeChange.prior]]). Otherwise no decision took another film and the store
 *  moved the listing: REGROUPED. */
object TakeCauses {

  /** What the projection read the takes against: the stored films before its writes, by id; whether a listing key, or a
   *  venue slot ([[StoredFilmSlots.slotOf]]), is published now; the listings re-published this update (none for an index
   *  built afresh); each listing's decision's members now; its trigger, and whether the store holds this process's takes. */
  final case class Reading(stored: String => Option[services.movies.StoredMovieRecord], published: ListingKey => Boolean,
                           publishedAt: ((String, String)) => Boolean, republished: Option[ListingKey => Boolean],
                           members: ListingKey => Seq[ListingKey], trigger: String, ownTakes: Boolean)

  /** The triggers that say an input beyond the listings moved: an answer filed, a stored film another writer moved. */
  val EvidenceTriggers: Set[String] = Set("answer", "film-change")

  /** `take`'s cause, for a move ([[TakeChange.moved]]); none for a new listing's first take. */
  def of(take: TakeChange, reading: Reading): Option[TakeChange.Cause] =
    take.previousFilm.filter(_ => take.moved).map { film =>
      val slots = reading.stored(film).toSeq.flatMap(IdentityProjectionPlan.slotsOf)
      val left  = slots.exists {
        case IdentityProjectionPlan.Slot(Some(at), _, key) => !reading.publishedAt(at) && key.forall(k => !reading.published(k))
        case IdentityProjectionPlan.Slot(None, _, key)     => key.exists(k => !reading.published(k))
      }
      val stoodOn = slots.iterator.flatMap(_.key) ++ reading.members(take.listing).iterator ++ Iterator(take.listing)
      val moved   = reading.republished.exists(again => stoodOn.exists(again))
      val asked   = reading.trigger.split('+').exists(EvidenceTriggers)
      if (left || moved) TakeChange.Cause.Evidence
      else if (!reading.ownTakes) TakeChange.Cause.Code
      else if (asked) TakeChange.Cause.Evidence
      else if (take.prior.isDefined) TakeChange.Cause.Code
      else TakeChange.Cause.Regrouped
    }
}

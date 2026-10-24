package services.identity

import services.movies.ListingKey

import java.util.Locale

/** A listing still published whose take a projection changed — THE change list every reader of what a projection changed
 *  reads (the take-change lines and counter, the staging screen, the drift check's kinds): one per listing, with its film
 *  and its decision's basis before and after.
 *
 *  A listing on a stored film (`previousFilm`, which held TMDB film `before`) whose cluster now takes another TMDB film
 *  (`after`) MOVED: `lost`, `gained` or `changed`. A listing on no stored film yet that a cluster now holds was DECIDED
 *  (`previousFilm` and `before` none, `after` the film it took or none). A listing that left the schedule has no cluster,
 *  and a re-key or merge that keeps the TMDB film changes no take: neither is one. `basisBefore` is the decision the
 *  projection replaced, when it saw it ([[LiveProjectionIndex.priorTake]]); `basisAfter` the one that holds it now. */
final case class TakeChange(listing: ListingKey, previousFilm: Option[String], before: Option[Int], after: Option[Int],
                            basisBefore: Option[ResolverDecision.Basis] = None, basisAfter: Option[ResolverDecision.Basis] = None) {
  def kind: TakeChange.Kind = (previousFilm, before, after) match {
    case (None, _, _)       => TakeChange.Kind.Decided
    case (_, Some(_), None) => TakeChange.Kind.Lost
    case (_, None, Some(_)) => TakeChange.Kind.Gained
    case _                  => TakeChange.Kind.Changed
  }

  /** A MATCH change — the live alarm's unit, what the take-change lines log and the reversal counts read — rather than a
   *  new listing's first take. */
  def moved: Boolean = previousFilm.isDefined
}

object TakeChange {
  enum Kind {
    case Decided, Lost, Changed, Gained
    def label: String = toString.toLowerCase(Locale.ROOT)
  }
  object Kind {
    /** The kinds a match change is: every one but [[Decided]]. */
    val Moves: Seq[Kind] = Seq(Lost, Changed, Gained)
  }

  /** The decision a listing's take came from before it moved: its basis, confidence and last reason, the rule that decided
   *  it ([[DecidedBy.of]]) and the runner-up its node weighed beside it (its TMDB id). */
  final case class Prior(basis: ResolverDecision.Basis, confidence: Double, reason: Option[String], decidedBy: Option[String] = None,
                         runnerUp: Option[Int] = None)

  /** Every listing of `previousOf` (a listing → the stored film it was on) whose cluster now (`filmOf`: none when the
   *  listing is in no cluster) takes another TMDB film than that film held, and every listing of `clustered` (each with
   *  its cluster's film) on no stored film, sorted by listing. One pass over the listings given, nothing kept: only the
   *  changes are allocated. */
  def of(previousOf: Map[ListingKey, StoredFilmRef], filmOf: ListingKey => Option[Option[Int]],
         clustered: Iterator[(ListingKey, Option[Int])]): Seq[TakeChange] = {
    val moved   = previousOf.iterator.flatMap { case (listing, was) =>
      filmOf(listing).filter(_ != was.tmdbId).map(now => TakeChange(listing, Some(was.id), was.tmdbId, now))
    }
    val decided = clustered.collect { case (listing, film) if !previousOf.contains(listing) => TakeChange(listing, None, None, film) }
    (moved ++ decided).toSeq.sortBy(_.listing)
  }
}

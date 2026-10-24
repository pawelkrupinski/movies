package services.identity

import services.movies.ListingKey

import java.util.Locale

/** A listing still published whose take a projection changed — THE change list every reader of what a projection changed
 *  reads (the take-change lines and counter, the staging screen, the drift check's kinds): one per listing, with its film
 *  and its decision's basis before and after, read off what the projection's writes did to the stored film the listing
 *  is on ([[LiveProjectionIndex.written]]).
 *
 *  A listing on a stored film (`previousFilm`, which held TMDB film `before`) whose cluster now takes another TMDB film
 *  (`after`) MOVED: `lost`, `gained` or `changed`. A listing on no stored film yet that a cluster now holds was DECIDED
 *  (`previousFilm` and `before` none, `after` the film it took or none) — `joined` when its cluster holds a listing on a
 *  stored film already (a re-scraped venue's "NAZA" joining the NAZA match), else a new cluster's. A listing that left
 *  the schedule has no cluster,
 *  and a re-key or merge that keeps the TMDB film changes no take: neither is one. `basisBefore` is the decision the
 *  projection replaced, when it saw it ([[LiveProjectionIndex.priorTake]]); `basisAfter` the one that holds it now. */
final case class TakeChange(listing: ListingKey, previousFilm: Option[String], before: Option[Int], after: Option[Int],
                            basisBefore: Option[ResolverDecision.Basis] = None, basisAfter: Option[ResolverDecision.Basis] = None,
                            joined: Boolean = false) {
  def kind: TakeChange.Kind = (previousFilm, before, after) match {
    case (None, _, _) if joined => TakeChange.Kind.Joined
    case (None, _, _)           => TakeChange.Kind.Decided
    case (_, Some(_), None)     => TakeChange.Kind.Lost
    case (_, None, Some(_))     => TakeChange.Kind.Gained
    case _                      => TakeChange.Kind.Changed
  }

  /** A MATCH change — the live alarm's unit, what the take-change lines log and the reversal counts read — rather than a
   *  new listing's first take. */
  def moved: Boolean = previousFilm.isDefined
}

object TakeChange {
  enum Kind {
    case Decided, Joined, Lost, Changed, Gained
    def label: String = toString.toLowerCase(Locale.ROOT)
  }
  object Kind {
    /** The kinds a match change is: every one but a new listing's ([[Decided]], [[Joined]]). */
    val Moves: Seq[Kind] = Seq(Lost, Changed, Gained)
  }

  /** The decision a listing's take came from before it moved: its basis, confidence and last reason, the rule that decided
   *  it ([[DecidedBy.of]]) and the runner-up its node weighed beside it (its TMDB id). */
  final case class Prior(basis: ResolverDecision.Basis, confidence: Double, reason: Option[String], decidedBy: Option[String] = None,
                         runnerUp: Option[Int] = None)
}

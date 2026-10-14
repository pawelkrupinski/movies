package services.identity

import services.movies.ListingKey

import java.util.Locale

/** A listing still published whose TMDB film a projection moved: it was on `previousFilm`, which held TMDB film
 *  `before`, and the resolver's cluster it is in now takes `after`. Neither side's absence is churn on its own — a
 *  listing that left the schedule has no cluster and a new listing no previous film, so neither is one; a re-key or
 *  merge that keeps the TMDB film is none either. What is left is a MATCH change: the live alarm's unit. */
final case class TakeChange(listing: ListingKey, previousFilm: String, before: Option[Int], after: Option[Int]) {
  def kind: TakeChange.Kind = (before, after) match {
    case (Some(_), None) => TakeChange.Kind.Lost
    case (None, Some(_)) => TakeChange.Kind.Gained
    case _               => TakeChange.Kind.Changed
  }
}

object TakeChange {
  enum Kind {
    case Lost, Changed, Gained
    def label: String = toString.toLowerCase(Locale.ROOT)
  }

  /** The decision a listing's take came from before it moved: its basis, confidence and last reason. */
  final case class Prior(basis: ResolverDecision.Basis, confidence: Double, reason: Option[String])

  /** Every listing of `previousOf` (a listing → the stored film it was on) whose cluster now (`filmOf`: none when the
   *  listing is in no cluster) takes another TMDB film than that film held, sorted by listing. One pass over the
   *  listings given, nothing kept: only the changes are allocated. */
  def of(previousOf: Iterable[(ListingKey, StoredFilmRef)], filmOf: ListingKey => Option[Option[Int]]): Seq[TakeChange] =
    previousOf.iterator.flatMap { case (listing, was) =>
      filmOf(listing).filter(_ != was.tmdbId).map(now => TakeChange(listing, was.id, was.tmdbId, now))
    }.toSeq.sortBy(_.listing)
}

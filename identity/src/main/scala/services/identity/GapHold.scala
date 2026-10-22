package services.identity

import services.movies.ListingKey

/**
 * NO TAKE MOVES ON EVIDENCE WITH GAPS. Right after a deploy brings new questions — and whenever a fill is behind — TMDB's
 * searches, records and venue pages are unanswered for a while, and a cluster decided then decides on what it can read:
 * it loses a take nothing contradicts (the record it would be compared with is a gap), or takes a film nothing can deny
 * yet (an IMDb suggestion, no record to compare), and the next fill moves it back. Production shows every such move as
 * a take change and back; a whole resolve over answered data never makes it (identity-slice --drift's gap states:
 * recording 573 PL, 160 listings moved and moved back by one deploy's gaps; DE "Rose" flipping to Babayan's film).
 *
 * So a decision still waiting on answers (`unanswered > 0`, or its family read an answer not known yet: the model's
 * `blind`) keeps each of its listings on the take it holds — film, fallback, basis — until the answers are in, the
 * decision split by the takes its listings held (a gap state merging two films' listings leaves each on its own) and
 * saying so. A listing with no take before (new) is decided as the cluster is. And for [[GapHold.Limit]] at most: a gap
 * that never closes holds nothing for good.
 */
final class GapHold(clock: java.time.Clock, limit: java.time.Duration = GapHold.Limit) {
  import GapHold.*

  /** When each listing held now began to be held: a hold lasts [[GapHold.Limit]] at most. */
  private val since = scala.collection.mutable.HashMap.empty[ListingKey, java.time.Instant]

  /** `model`'s decisions, each one waiting on answers that would move its listings off their `prior` take holding it —
   *  for [[GapHold.Limit]] at most: an answer that never comes (a question that fails for good) holds nothing for ever. */
  def apply(model: Resolution, prior: ListingKey => Option[Prior], blind: ListingKey => Boolean = _ => false): Resolution = synchronized {
    val now  = clock.instant()
    val kept = scala.collection.mutable.HashSet.empty[ListingKey]
    def within(members: Seq[ListingKey]) =
      !now.isAfter(members.iterator.map(k => since.getOrElse(k, now)).minOption.getOrElse(now).plus(limit))
    val decisions = model.decisions.flatMap { d =>
      val parts = held(d, prior, blind)
      if (parts.size == 1 && (parts.head eq d)) parts
      else {
        // a part holding a take past the limit decides as the cluster decided
        val (holding, expired) = parts.filterNot(_ eq d).partition(part => within(part.members))
        holding.foreach(_.members.foreach { k => kept += k; since.getOrElseUpdate(k, now) })
        val rest = d.members.filterNot(holding.flatMap(_.members).toSet)
        holding ++ Option.when(rest.nonEmpty)(if (rest.size == d.members.size) d else d.copy(members = rest)(d.trace))
      }
    }
    since.filterInPlace((k, _) => kept(k))
    if (kept.isEmpty) model else model.copy(decisions = decisions)
  }
}

object GapHold {
  /** How long a take is held at most: a deploy's new questions are filed within the hour; a gap still open after two is
   *  one that will not close, and the decision stands as a whole resolve would take it. */
  val Limit: java.time.Duration = java.time.Duration.ofHours(2)

  /** The take a listing held: its film, fallback film and the basis it stood on. */
  final case class Prior(film: Option[Int], fallback: Option[ResolverDecision.Fallback], basis: ResolverDecision.Basis, confidence: Double) {
    /** Does `d` take the same film — the same TMDB film, or a fallback film of the same source and id? */
    def same(d: ResolverDecision): Boolean = film == d.film && fallback.map(f => (f.source, f.id)) == d.fallback.map(f => (f.source, f.id))
    def take: (Option[Int], Option[(String, String)]) = (film, fallback.map(f => (f.source, f.id)))
  }
  object Prior {
    def of(d: ResolverDecision): Prior = Prior(d.film, d.fallback, d.basis, d.confidence)
    /** What a stored film shows: its TMDB film, else the fallback film its record names (its IMDb id, its Wikidata entity,
     *  its Filmweb page), else none — the take a worker just up finds a listing on. */
    def stored(tmdbId: Option[Int], row: Option[services.movies.StoredMovieRecord]): Prior = tmdbId match {
      case Some(_) => Prior(tmdbId, None, ResolverDecision.Basis.OwnMatch, 1.0)
      case None =>
        val fallback = row.flatMap { r =>
          r.record.imdbId.map(id => ResolverDecision.Fallback("imdb", id, 1.0))
            .orElse(r.record.wikidataId.map(id => ResolverDecision.Fallback("wikidata", id, 1.0, Some(r.title), r.year)))
            .orElse(r.record.filmwebUrl.flatMap(url => "(\\d+)/?$".r.findFirstMatchIn(url).map(_.group(1)))
              .map(id => ResolverDecision.Fallback("filmweb", id, 1.0, Some(r.title), r.year)))
        }
        Prior(None, fallback, if (fallback.isDefined) ResolverDecision.Basis.Agreed else ResolverDecision.Basis.NoCandidate, 1.0)
    }
  }

  /** The line a held decision adds to its explanation. */
  def line(prior: Prior, unanswered: Int): String =
    s"held: ${prior.film.map(id => s"tmdb $id").orElse(prior.fallback.map(f => s"${f.source}:${f.id}")).getOrElse("no film")} kept " +
      (if (unanswered > 0) s"while $unanswered question(s) it reads are unanswered" else "while an answer it reads is not known yet")

  /** `d` split by the take its listings held, where it waits on answers — an unanswered search (`unanswered`), or any
   *  answer its family met unknown (`blind`: a film's record, a venue page): each group of its listings that held one
   *  take it would move them off keeps that take, the rest stay `d`'s. `Seq(d)` where nothing is held. */
  def held(d: ResolverDecision, prior: ListingKey => Option[Prior], blind: ListingKey => Boolean = _ => false): Seq[ResolverDecision] =
    if (d.members.isEmpty || (d.unanswered <= 0 && !d.members.exists(blind))) Seq(d)
    else {
      val moved = d.members.flatMap(k => prior(k).filterNot(_.same(d)).map(k -> _))
      if (moved.isEmpty) Seq(d)
      else {
        val groups = moved.groupBy { case (_, p) => p.take }.values.toSeq.sortBy(_.head._1)(using ListingKey.ordering)
        val held   = groups.map { group =>
          val only = group.head._2
          d.copy(members = group.map(_._1), film = only.film, fallback = only.fallback, basis = only.basis, confidence = only.confidence,
            explanation = d.explanation :+ line(only, d.unanswered))(d.trace)
        }
        val rest = d.members.filterNot(moved.map(_._1).toSet)
        held ++ Option.when(rest.nonEmpty)(d.copy(members = rest)(d.trace))
      }
    }
}

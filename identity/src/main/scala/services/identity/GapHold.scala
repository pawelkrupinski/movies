package services.identity

import services.movies.ListingKey

import scala.collection.mutable

/**
 * NO ESTABLISHED TAKE MOVES ON EVIDENCE WITH GAPS. Right after a deploy brings new questions — and whenever a fill is
 * behind — TMDB's searches, records and venue pages are unanswered for a while, and a cluster decided then decides on
 * what it can read: it loses a take nothing contradicts (the record it would be compared with is a gap), or takes a film
 * nothing can deny yet (an IMDb suggestion, no record to compare), and the next fill moves it back. Production shows every
 * such move as a take change and back; a whole resolve over answered data never makes it (identity-slice --drift's gap
 * states: recording 573 PL, 160 listings moved and moved back by one deploy's gaps; DE "Rose" flipping to Babayan's film).
 *
 * So a decision still waiting on answers (`unanswered > 0`, or its family read an answer not known yet: the model's
 * `blind`) keeps each of its listings on the ESTABLISHED take it holds — film, fallback, basis — the decision split by the
 * takes its listings held, and saying so. Established is:
 *  - a take the last projection made on answered evidence (its decision waited on nothing), or one it held;
 *  - at a worker's start, what its stored film shows (the take the site serves).
 * Never an initial take (a listing with none before: new, or a boot's over an empty store), and never one itself made on
 * gaps: what a boot or an outage took while answers were missing moves freely as they arrive.
 *
 * A hold ends when its decision has its answers (it waits on nothing, and decides as it decides); else once the lookup
 * fill has ended [[GapHold.Rounds]] complete rounds since it began — every question it could ask asked, so a gap still
 * open (a search TMDB answers 404, unknown for good) is one no fill closes; and at the latest after [[GapHold.Limit]],
 * so a fill that never runs holds nothing for good. A pin is never held.
 *
 * Free on a quiet tick: a decision whose listings hold the take it makes is passed on as it is, nothing allocated, and
 * the resolution is copied only when a take is held.
 */
final class GapHold(clock: java.time.Clock, limit: java.time.Duration = GapHold.Limit) {
  import GapHold.*

  /** When each listing held now began to be held, and how many complete fill rounds had ended by then. */
  private val since = mutable.HashMap.empty[ListingKey, Began]
  /** The listings the last call's model had decided on an answer not known yet: was a take it made established? */
  private var lastBlind: ListingKey => Boolean = _ => false
  /** The listings whose established take a decision waiting on answers made again — a take a gap state left as it was
   *  stays established (a deploy's first projection over the stored films, then the next while the fill is half done):
   *  each by the decision that confirmed it, so a take that moves since is not. */
  private val confirmed = mutable.HashMap.empty[ListingKey, ResolverDecision]

  /** A listing's established take — `taken`, the decision its take came from last time — unless `d` takes the same film:
   *  `null` where there is nothing to hold (no take before, the same one, or one made on gaps). */
  def priorOf(key: ListingKey, taken: ResolverDecision, d: ResolverDecision): Prior =
    if (taken == null || same(taken, d) || !established(key, taken)) null
    else Prior.of(taken)

  /** Does `d` make again the established take `taken` holds `key` on? */
  def confirms(key: ListingKey, taken: ResolverDecision, d: ResolverDecision): Boolean =
    taken != null && same(taken, d) && established(key, taken)

  private def established(key: ListingKey, taken: ResolverDecision): Boolean =
    since.contains(key) || (taken.unanswered <= 0 && !lastBlind(key)) || { val c = confirmed.getOrElse(key, null); c != null && same(c, taken) }

  /**
   * `model`'s decisions, each one waiting on answers that would move its listings off an established take holding it.
   * `prior(key, d)` is the listing's established take that `d` would move it off, `null` for none ([[priorOf]]);
   * `confirms(key, d)` whether `d` makes the listing's established take again ([[confirms]]); `fillRounds` how many
   * complete rounds the lookup fill has ended so far.
   */
  def apply(model: Resolution, prior: (ListingKey, ResolverDecision) => Prior, confirms: (ListingKey, ResolverDecision) => Boolean,
            blind: ListingKey => Boolean, fillRounds: Long): Resolution = synchronized {
    val now  = clock.instant()
    var kept: mutable.HashSet[ListingKey]           = null
    var out:  mutable.ArrayBuffer[ResolverDecision] = null
    def within(members: Seq[ListingKey]): Boolean = members.forall(k => since.get(k).forall(b =>
      !now.isAfter(b.at.plus(limit)) && fillRounds < b.rounds + Rounds))
    val ds = model.decisions
    var i  = 0
    val it = ds.iterator
    while (it.hasNext) {
      val d       = it.next()
      val waiting = waits(d, blind)
      if (waiting) d.members.foreach(k => if (confirms(k, d)) confirmed.update(k, d))
      else if (confirmed.nonEmpty) d.members.foreach(confirmed.subtractOne)
      if (waiting && moves(d, prior)) {
        // a part held past its rounds or its limit decides as the cluster decided
        val holding = held(d, prior).filter(part => within(part.members))
        val keys    = holding.iterator.flatMap(_.members).toSet
        val rest    = d.members.filterNot(keys)
        if (holding.nonEmpty) {
          if (out == null) { out = mutable.ArrayBuffer.empty; out ++= ds.iterator.take(i) }
          if (kept == null) kept = mutable.HashSet.empty
          holding.foreach(_.members.foreach { k => kept += k; since.getOrElseUpdate(k, Began(now, fillRounds)) })
          out ++= holding
          if (rest.nonEmpty) out += (if (rest.size == d.members.size) d else d.copy(members = rest)(d.trace))
        } else if (out != null) out += d
      } else if (out != null) out += d
      i += 1
    }
    if (since.nonEmpty) { if (kept == null) since.clear() else since.filterInPlace((k, _) => kept(k)) }
    lastBlind = blind
    if (out == null) model else model.copy(decisions = out.toVector)
  }
}

object GapHold {
  /** How long a take is held at most: a deploy's new questions are filed within the hour; a gap still open after two is
   *  one that will not close, and the decision stands as a whole resolve would take it. */
  val Limit: java.time.Duration = java.time.Duration.ofHours(2)

  /** How many complete lookup fill rounds a hold outlasts: the first files the answers a deploy's questions waited on,
   *  the second asks what those answers named (a record a search found) once the model has read them. */
  val Rounds: Long = 2L

  private final case class Began(at: java.time.Instant, rounds: Long)

  /** The take a listing held: its film, fallback film and the basis it stood on. */
  final case class Prior(film: Option[Int], fallback: Option[ResolverDecision.Fallback], basis: ResolverDecision.Basis, confidence: Double) {
    /** Does `d` take the same film — the same TMDB film, or a fallback film of the same source and id? */
    def same(d: ResolverDecision): Boolean = GapHold.same(film, fallback, d)
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

  /** Do `a` and `b` take the same film — the same TMDB film, or a fallback film of the same source and id? */
  def same(a: ResolverDecision, b: ResolverDecision): Boolean = same(a.film, a.fallback, b)
  private def same(film: Option[Int], fallback: Option[ResolverDecision.Fallback], d: ResolverDecision): Boolean =
    film == d.film && (fallback match {
      case None    => d.fallback.isEmpty
      case Some(f) => d.fallback.exists(g => g.source == f.source && g.id == f.id)
    })

  /** Does `d` wait on answers: an unanswered search, or an answer its family met unknown? A pin waits on nothing. */
  def waits(d: ResolverDecision, blind: ListingKey => Boolean): Boolean =
    d.basis != ResolverDecision.Basis.Pinned && d.members.nonEmpty && (d.unanswered > 0 || d.members.exists(blind))

  /** Would `d` move a listing off an established take? */
  private def moves(d: ResolverDecision, prior: (ListingKey, ResolverDecision) => Prior): Boolean =
    d.members.exists(k => prior(k, d) != null)

  /** The line a held decision adds to its explanation. */
  def line(prior: Prior, unanswered: Int): String =
    s"held: ${prior.film.map(id => s"tmdb $id").orElse(prior.fallback.map(f => s"${f.source}:${f.id}")).getOrElse("no film")} kept " +
      (if (unanswered > 0) s"while $unanswered question(s) it reads are unanswered" else "while an answer it reads is not known yet")

  /** The parts of `d` its listings' established takes hold: each group of its listings that held one take `d` would move
   *  them off keeps that take. None where nothing is held. */
  def held(d: ResolverDecision, prior: (ListingKey, ResolverDecision) => Prior): Seq[ResolverDecision] = {
    val moved = d.members.flatMap(k => Option(prior(k, d)).map(k -> _))
    moved.groupBy { case (_, p) => p.take }.values.toSeq.sortBy(_.head._1)(using ListingKey.ordering).map { group =>
      val only = group.head._2
      d.copy(members = group.map(_._1), film = only.film, fallback = only.fallback, basis = only.basis, confidence = only.confidence,
        explanation = d.explanation :+ line(only, d.unanswered))(d.trace)
    }
  }
}

package services.identity

import services.identity.FamilyClosure.Edge
import services.movies.ListingConstraints.{CannotLink, MustLink}
import services.movies.ListingKey

/**
 * The pin set as the resolver reads it — HARD constraints, stronger than every derived rule
 * (docs/design/identity-resolver.md, "Phase 3: curation"). Reached through
 * `ListingConstraints.pinned`, so pins enter the one constraint model like every other rule.
 *
 * What the resolver does with it:
 *  - [[resolvedFilm]] replaces a listing's own lookup answer: a pinned film wins, a denied one
 *    is dropped;
 *  - [[blockKeys]] are added to the listing's family block keys, so every pin edge joins two
 *    listings of one family (the closure stays complete, `FamilyClosure.check`);
 *  - [[mustLinks]] go in as the strongest tier, [[cannotLinks]] beside the derived ones;
 *  - [[admits]] filters the DERIVED edges: a cannot-link inside a pinned group, or a must-link
 *    onto a film the pins deny, is dropped — the pin overrides the rule.
 *
 * A pure function of the pin SET: groups and edges come out sorted, whatever order the pins do.
 */
final case class PinConstraints(pins: Seq[Pin]) {

  /** Listings pinned together (by a same-film pin, or pinned to one film), each group sorted,
   *  keyed by its smallest listing. */
  private val groupOf: Map[ListingKey, ListingKey] = {
    val parent = scala.collection.mutable.HashMap.empty[Either[Int, ListingKey], Either[Int, ListingKey]]
    def find(x: Either[Int, ListingKey]): Either[Int, ListingKey] = {
      val p = parent.getOrElseUpdate(x, x)
      if (p == x) x else { val r = find(p); parent(x) = r; r }
    }
    def union(a: Either[Int, ListingKey], b: Either[Int, ListingKey]): Unit = { val (ra, rb) = (find(a), find(b)); if (ra != rb) parent(ra) = rb }
    pins.foreach { pin =>
      val keys = pin.listings.map(Right(_))
      keys.foreach(find)
      pin.claim match {
        case PinClaim.IsFilm(id)   => keys.foreach(union(_, Left(id)))
        case PinClaim.SameFilm     => keys.drop(1).foreach(union(_, keys.head))
        case PinClaim.NeverFilm(_) => ()
      }
    }
    val listings = parent.keys.collect { case Right(k) => k }.toSeq
    listings.groupBy(k => find(Right(k))).values.flatMap { members =>
      val head = members.min
      members.map(_ -> head)
    }.toMap
  }

  private val groups: Map[ListingKey, Seq[ListingKey]] =
    groupOf.toSeq.groupMap(_._2)(_._1).view.mapValues(_.sorted).toMap

  private def members(k: ListingKey): Seq[ListingKey] = groupOf.get(k).fold(Seq(k))(groups)

  private val pinnedFilms: Map[ListingKey, Set[Int]] =
    pins.flatMap(p => p.claim match {
      case PinClaim.IsFilm(id) => p.listings.map(groupOf(_) -> id)
      case _                   => Nil
    }).groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap

  private val deniedByGroup: Map[ListingKey, Set[Int]] =
    pins.flatMap(p => p.claim match {
      case PinClaim.NeverFilm(id) => p.listings.map(k => groupOf.getOrElse(k, k) -> id)
      case _                      => Nil
    }).groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap

  /** The film the pins say `k` is — when they name exactly one. */
  def filmOf(k: ListingKey): Option[Int] =
    groupOf.get(k).flatMap(pinnedFilms.get).filter(_.sizeIs == 1).map(_.head)

  /** The films the pins say `k` never is (its whole group's). */
  def deniedFilms(k: ListingKey): Set[Int] = deniedByGroup.getOrElse(groupOf.getOrElse(k, k), Set.empty)

  /** `k`'s film once the pins have spoken: the pinned film, else its own lookup answer unless
   *  a pin denies it. */
  def resolvedFilm(k: ListingKey, looked: Option[Int]): Option[Int] =
    filmOf(k).orElse(looked.filterNot(deniedFilms(k)))

  /** Extra family block keys: the group's own key, and the pinned and denied films' TMDB keys
   *  (the same `id:` form `FamilyClosure.blockKeys` gives a resolved listing). */
  def blockKeys(k: ListingKey): Set[String] =
    groupOf.get(k).map(head => s"pin:$head").toSet ++ (filmOf(k).toSet ++ deniedFilms(k)).map(id => s"id:$id")

  /** Every pinned group as a star from its smallest listing. */
  lazy val mustLinks: Seq[Edge[ListingKey]] =
    groups.toSeq.sortBy(_._1).flatMap { case (head, ms) =>
      ms.filterNot(_ == head).map(Edge(head, _, must = true, reason = MustLink.Pinned.toString))
    }

  /** Each listing a pin denies film X, cannot-linked from every one of `keys` whose film
   *  (`filmOf`, the resolver's per-listing answer after [[resolvedFilm]]) is X. */
  def cannotLinks(keys: Seq[ListingKey], filmOf: ListingKey => Option[Int]): Seq[Edge[ListingKey]] = {
    val sorted = keys.distinct.sorted
    for {
      k    <- sorted if deniedFilms(k).nonEmpty
      j    <- sorted if j != k && filmOf(j).exists(deniedFilms(k))
    } yield Edge(k, j, must = false, reason = CannotLink.PinnedNotFilm.toString)
  }

  /** Whether a DERIVED edge survives the pins. */
  def admits(edge: Edge[ListingKey]): Boolean =
    if (!edge.must) !(groupOf.contains(edge.a) && groupOf.get(edge.a) == groupOf.get(edge.b))
    else {
      val (fa, fb) = (filmOf(edge.a), filmOf(edge.b))
      !(fa.isDefined && fb.isDefined && fa != fb) &&
        !fa.exists(deniedFilms(edge.b)) && !fb.exists(deniedFilms(edge.a))
    }

  /** Pins that contradict each other, in words: a group pinned to two films, or to a film one of
   *  its listings is pinned never to be. Empty for a consistent set. */
  lazy val conflicts: Seq[String] =
    groups.keys.toSeq.sorted.flatMap { head =>
      val films  = pinnedFilms.getOrElse(head, Set.empty)
      val denied = deniedFilms(head)
      Option.when(films.sizeIs > 1)(s"${members(head).mkString(", ")} pinned to films ${films.toSeq.sorted.mkString(" and ")}") ++
        Option.when((films & denied).nonEmpty)(
          s"${members(head).mkString(", ")} pinned both to and never to film ${(films & denied).toSeq.sorted.mkString(", ")}")
    }
}

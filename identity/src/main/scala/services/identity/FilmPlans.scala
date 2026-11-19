package services.identity

import models.{Cinema, ListingKey}

import IdentityProjectionPlan.VenueGroup

/** The films one draft ([[ScopeDrafting]]) plans, by the ids `assigned` gave them over `index`: each film's venues'
 *  groups, kept from its last draft ([[FilmShapes]]) where nothing they are built from moved — of a scoped draft, whose
 *  `changed` listings say what did — else worked out. */
private[identity] final class FilmPlans(index: ProjectionIndex, scope: ProjectionScope, shapes: FilmShapes,
                                        slotSource: VenueSlotSource, changed: Set[ListingKey], assigned: IdAssigner.Assignment) {
  import index.{byKey, storedById, covered}
  import FilmPlans.{Planned, PlannedGroup}

  private def priorsOf(id: String): Map[Cinema, Int] = storedById.get(id).fold(Map.empty[Cinema, Int])(r => shapes.priorsOf(id, r.record))
  // One venue's listings on a film, and what its slots are built from.
  private def previousAt(keys: Seq[ListingKey]): Seq[Option[String]] = keys.map(k => index.previousOf.get(k).map(_.id))
  private def priorsAt(cinema: Cinema, previous: Seq[Option[String]]): Seq[Int] =
    previous.flatten.distinct.sorted.map(id => priorsOf(id).getOrElse(cinema, 0))
  // What the venue's pages for these listings said when last read: a slot carries its page-written detail from them.
  private def pagesAt(cinema: Cinema, keys: Seq[ListingKey]): Int = slotSource.pagesDigest(cinema, keys)
  private def groupOf(cinema: Cinema, ofVenue: Seq[ProjectedListing], counter: Long): PlannedGroup = {
    // The prior slots any of these rows' previous films held at the venue: a superset of the one each built
    // slot carries forward, so a change to any of them is a change to the key — and which film each row is on,
    // since a slot carries forward its own listing's film's slot: two rows trading films trade the slot each
    // carries, the set of priors unchanged. Left out, a slot built over one film's prior was taken from the memo
    // for the other's (a venue's "Lalka" and "Lalka 2D" trading films kept the director only one of them had).
    val previous = previousAt(ofVenue.map(_.listing.key))
    val priors   = priorsAt(cinema, previous)
    // Each row's film by its counter, which a film keeps for good and has from its first draft: what the key of a
    // slot once written is worked out from ([[VenueSlotMemo.written]]), a film new to the store included.
    (VenueGroup(cinema, ofVenue, VenueSlotMemo.keyOf(cinema.displayName, ofVenue, priors, previous.map(_.flatMap(covered.counterOf)),
      pagesAt(cinema, ofVenue.map(_.listing.key))),
      counter), previous, priors)
  }

  // A film drafted again with the listings it was last drafted with is drafted again only where it moved: a venue one of
  // whose listings, or whose listings' previous films' stored slots, moved since ([[FilmShapes]]).
  private val reuse   = !scope.whole
  private lazy val placedAt: Map[ListingKey, Long] = assigned.idOfListing
  private val dirtyAt = if (!reuse) Map.empty[Long, Set[ListingKey]]
                        else changed.iterator.filter(byKey.contains).flatMap(k => placedAt.get(k).map(_ -> k)).toSeq.groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap

  /** Each film's plan: its venues' groups, kept from its last draft where nothing they are built from moved. */
  def all: Seq[Planned] = assigned.ids.map { case (counter, members) =>
    // The same listings: the same keys in the same order, the same titles (a key holds its listing's title). An equal set
    // of other key objects is the same listings read again — a venue's scrape reads every listing it prints, moved or not.
    shapes.get(counter).filter(sh => reuse && ((sh.members eq members) || sh.members == members))
      .map(_.over(members, k => byKey.get(k).fold(k)(_.listing.key))) match {
      case Some(was) => plannedAgain(counter, members, was)
      case _         => plannedAnew(counter, members)
    }
  }

  /** A film drafted again with the listings it was last drafted with: its venues that moved are worked out again. */
  private def plannedAgain(counter: Long, members: Set[ListingKey], was: FilmShape): Planned = {
    val dirtyVenues = dirtyAt.getOrElse(counter, Set.empty).map(byKey(_).listing.cinema)
    val groups = was.venues.toSeq.sortBy(_._1.displayName).map { case (cinema, venue) =>
      // Moved: one of its listings did, or which film one is on, or the slots those films hold at the venue, or
      // what one's page said.
      val moved = dirtyVenues(cinema) || { val previous = previousAt(venue.keys)
        VenueShape.inputs(previous, priorsAt(cinema, previous), pagesAt(cinema, venue.keys)) != venue.inputs }
      if (!moved) cinema -> Left(venue)
      else cinema -> Right(groupOf(cinema, venue.keys.map(byKey), counter))
    }
    Planned(counter, members, was.keys, was.titles, groups, Some(was))
  }

  /** A film drafted with other listings than last time, or for the first time: every venue worked out. */
  private def plannedAnew(counter: Long, members: Set[ListingKey]): Planned = {
    val keys   = members.toSeq.sorted
    val rows   = keys.map(byKey)
    val titles = rows.groupMapReduce(_.listing.cleanTitle)(_ => 1)(_ + _)
    val groups = rows.groupBy(_.listing.cinema).toSeq.sortBy(_._1.displayName).map { case (cinema, ofVenue) =>
      cinema -> Right(groupOf(cinema, ofVenue, counter))
    }
    Planned(counter, members, keys, titles, groups, None)
  }
}

private[identity] object FilmPlans {
  /** A venue group worked out: the group, and the films its listings were on and the slots those held at the venue. */
  type PlannedGroup = (VenueGroup, Seq[Option[String]], Seq[Int])

  /** A film [[ScopeDrafting]] plans: its venues' groups, each one kept from the last draft (`Left`) or
   *  worked out (`Right`, with the stored records of the films its priors are from). */
  final case class Planned(counter: Long, members: Set[ListingKey], keys: Seq[ListingKey], titles: Map[String, Int],
                           groups: Seq[(Cinema, Either[VenueShape, PlannedGroup])], was: Option[FilmShape])
}

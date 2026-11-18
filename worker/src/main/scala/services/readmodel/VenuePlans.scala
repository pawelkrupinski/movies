package services.readmodel

import services.movies.ChangeStreamMetrics.VenueDecline as Why

/** How [[ReadModelProjector.onVenueSlots]] plans a venue-only apply: the venues it rebuilds, by card, or the first
 *  reason it cannot vouch that they are all a whole-film projection would change. */
private[readmodel] object VenuePlans {
  /** Why the plan declines, or the venues it rebuilds, by card. */
  type Plan = Either[String, Map[String, Seq[ReadModelProjection.VenueScreening]]]

  def refuse(reason: String)(using scala.util.boundary.Label[Plan]): Nothing = scala.util.boundary.break(Left(reason))

  /** One venue's rebuilt screening per card it shows — refused unless each is a screening the card already serves
   *  (`served(card, id)`) under the same id, from one slot. */
  def ofCinema(groups: RowGroups, cards: Seq[String], cinema: models.Cinema,
               slots: Seq[(models.CinemaShowing, models.SourceData)], sanitize: String => String,
               served: (String, String) => Boolean)
              (using scala.util.boundary.Label[Plan]): Seq[(String, ReadModelProjection.VenueScreening)] = {
    val byCard = slots.map { case (_, slot) =>
      groups.cardByGroup.getOrElse(slot.title.map(sanitize).getOrElse(groups.anchor), refuse(Why.ProjectorTitleGroup)) -> slot
    }.groupMap(_._1)(_._2)
    if (byCard.exists(_._2.sizeIs > 1)) refuse(Why.ProjectorTwoSlots)
    cards.flatMap { card =>
      val before = models.City.forCinema(cinema).map(ReadModelProjection.screeningId(card, _, cinema)).filter(served(card, _))
      val after  = byCard.get(card).flatMap(slots => ReadModelProjection.venueOf(cinema, slots.head, card))
      (before, after) match {
        case (None, None)                                  => None
        case (None, Some(_))                               => refuse(Why.ProjectorVenueAppears)
        case (Some(_), None)                               => refuse(Why.ProjectorVenueVanishes)
        case (Some(id), Some(venue)) if venue._id != id || groups.multiSlot(id) => refuse(Why.ProjectorUnionedVenue)
        case (Some(_), Some(venue))                        => Some(card -> venue)
      }
    }
  }
}

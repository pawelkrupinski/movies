package models

/** The order the covered-town lists share: most venues first, ties alphabetical.
 *
 *  [[City.coveredPlaces]] promises its consumers that order, because they cannot
 *  all use the whole list — a page heading and a meta description have room for
 *  a handful of names, and the towns worth spending them on are the ones with
 *  the cinemas. Shared because the rosters that count their own rank the same
 *  way: the US off its venues' towns (`UsRoster.townsOf`), the UK and Poland off
 *  the venue table ([[VenueTowns.of]]). Germany and Spain arrive pre-ranked from
 *  their generators, which sort by this key on the Python side. */
private[models] object TownRanking {

  /** Rank one town name per venue into the distinct towns, biggest first.
   *  Blanks drop out: a venue whose town we do not know must not become a town
   *  called "". */
  def ranked(perVenue: Seq[String]): Seq[String] =
    perVenue.filter(_.nonEmpty).groupBy(identity).toSeq
      .sortBy { case (town, sharing) => (-sharing.size, town) }
      .map(_._1)
}

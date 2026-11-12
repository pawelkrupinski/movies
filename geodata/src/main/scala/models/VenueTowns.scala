package models

/** The town each venue of a HAND-WRITTEN roster sits in — the roster fact
 *  Germany, Spain and the US carry in their generated rosters, and the UK and
 *  Poland, whose venues are hand-written, have to keep beside them. Read by
 *  `TownRanking.ofVenues` (behind `City.extraPlaces`), so `/london/` names Croydon and `/tarnow/`
 *  names Bochnia rather than each naming only itself. The table is
 *  `generated/venue-towns.tsv`, which `data/scripts/generate_venue_towns.py` writes from
 *  data/uk/venues.json + data/pl/venues.json; regenerate after rebuilding either country's table. */
private[models] object VenueTowns {

  /** Venue display name -> its town. Display name because that is the key a
   *  venue is identified by everywhere else — `Source.byDisplayName`, the
   *  stored slots, the scrape catalog. */
  val byDisplayName: Map[String, String] = GeneratedTable.lookup("generated/venue-towns.tsv")
}

package models

/** The districts of a US metro too big to browse as one list — a second level of
 *  grouping under a state's metros (`UsRoster.metroAreas`), for the five metros
 *  that clear `cluster_metros.MIN_VENUES_TO_SUBDIVIDE`: Los Angeles (133
 *  venues), New York (102), San Francisco (79), Chicago and Dallas Fort Worth
 *  (78 each).
 *
 *  London splits the same way conceptually but by COMPASS — Central / North /
 *  East / South / West, hand-written across its 133 venues. That is the wrong
 *  tool here twice over. It does not scale: these metros hold 470 venues and the
 *  roster is regenerated from `venues.json` on every re-harvest, so a hand map
 *  would rot. And it does not describe a US metro: measured on the real
 *  coordinates, a compass split put Manhattan half in "West" and half in
 *  "Central", left New York a five-venue "South", and — because the Bay is in
 *  the middle of the Bay Area and Lake Michigan is east of Chicago — gave San
 *  Francisco a "Central" of six East Bay venues and filed the Loop under "East".
 *  A metro's shape is not radial, so no anchor or radius fixes that.
 *
 *  So a district is named after the PLACE it is, exactly as a metro is: the
 *  generator clusters the metro's venues again at a twelfth of the metro radius
 *  and names each cluster after its dominant town. For New York the towns ARE
 *  the boroughs, so Manhattan, Brooklyn, The Bronx and Staten Island fall
 *  straight out of the data; for Los Angeles it is Santa Monica, Pasadena,
 *  Burbank, Long Beach. See `data/us/scripts/cluster_metros.py`.
 *
 *  This side is only the grouping. The clustering is the generator's, the label
 *  is carried per venue in `UsRosterData`, and the slug is re-derived from the
 *  label by `Slugify.stable` — the frozen fold clients persist an area under. */
object UsMetroSubAreas {

  /** This metro's venues grouped into its districts, biggest first — or `Nil`
   *  when it is not one of the sub-divided metros. Keyed by the state's
   *  `City.slug` and the metro's `CinemaArea.slug`, the two segments a
   *  `/{state}/{metro}/` page already has.
   *
   *  A pure function of the roster: same roster, byte-identical answer. */
  def forMetro(stateSlug: String, metroSlug: String): Seq[CinemaAreaGroup] =
    byMetro.getOrElse((stateSlug, metroSlug), Nil)

  /** Computed once for every sub-divided metro — five of the 470, so the whole
   *  table costs one pass over 470 venues at class-load.
   *
   *  A metro takes part only when EVERY one of its venues carries a district;
   *  the groups have to partition it, and half a metro grouped is worse than
   *  none. Nothing in the generated roster can hit that — the generator labels a
   *  metro's venues all together or not at all — but the lookup has to answer
   *  something, and "not sub-divided" is the answer that keeps the invariant. */
  private val byMetro: Map[(String, String), Seq[CinemaAreaGroup]] =
    UsRoster.regions.flatMap { region =>
      region.areas.flatMap { metro =>
        val districts = metro.cinemas.flatMap(c => UsRoster.subAreaByCinema.get(c).map(c -> _))
        Option.when(districts.sizeIs == metro.cinemas.size)(
          (region.slug, metro.area.slug) -> UsRoster.areasByLabel(districts))
      }
    }.toMap
}

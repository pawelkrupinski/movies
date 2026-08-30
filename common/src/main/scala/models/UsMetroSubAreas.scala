package models

/** The districts of a US metro too big to browse as one list — the `areas` of
 *  the five metro [[City]]s that clear `cluster_metros.MIN_VENUES_TO_SUBDIVIDE`:
 *  Los Angeles (133 venues), New York (102), San Francisco (79), Chicago and
 *  Dallas Fort Worth (78 each).
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
 *  Three of the five then fold those districts onto REGIONS
 *  (`cluster_metros.SUB_AREA_REGIONS`), because a town name is only right while
 *  the district really is inside the metro's namesake city. It is for Chicago,
 *  whose suburbs are all Chicagoland, and for Dallas Fort Worth, already named
 *  after both its anchors — neither folds. It is not for the three that sprawl
 *  across places with identities of their own, where the raw list asserts a
 *  containment no resident would say: San Jose is not inside San Francisco,
 *  Long Island is not inside New York, and Orange County is not inside Los
 *  Angeles. Those fold onto the regions a local browses by — East Bay, South
 *  Bay, Peninsula, North Bay; Queens, Long Island, Westchester, Rockland; the
 *  San Fernando and San Gabriel Valleys, the Westside, the South Bay, the
 *  Gateway Cities.
 *
 *  Still not a compass split — the fold is over the districts the clustering
 *  already found, so the geography is unchanged and only the buckets are
 *  renamed. Which also caps its precision at the district: the 6 km sub-pass
 *  puts two Queens venues across the river in the Manhattan cluster, so both
 *  land in Manhattan.
 *
 *  This side is only the grouping. The clustering is the generator's, the label
 *  is carried per venue in `UsRosterData`, and the slug is re-derived from the
 *  label by `Slugify.stable` — the frozen fold clients persist an area under. */
object UsMetroSubAreas {

  /** One metro's venues grouped into its districts, biggest first — or `Nil`
   *  when it is not one of the sub-divided metros. `venues` pairs each of the
   *  metro's cinemas with the district label the generator gave it, empty for
   *  a metro it left whole.
   *
   *  A metro takes part only when EVERY one of its venues carries a district;
   *  the groups have to partition it, and half a metro grouped is worse than
   *  none. Nothing in the generated roster can hit that — the generator labels a
   *  metro's venues all together or not at all — but the rule has to answer
   *  something, and "not sub-divided" is the answer that keeps the invariant.
   *
   *  A pure function of the roster: same roster, byte-identical answer. */
  private[models] def districts(venues: Seq[(Cinema, String)]): Seq[CinemaAreaGroup] =
    if (venues.exists(_._2.isEmpty)) Nil else CinemaAreaGroup.byLabel(venues)
}

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
 *  San Francisco is the one metro that then folds those districts onto REGIONS
 *  (`cluster_metros.SUB_AREA_REGIONS`), because it is the one metro that is not
 *  a city. Manhattan is inside New York and Pasadena is inside Los Angeles, but
 *  San Jose is not inside San Francisco — listed by their dominant towns the
 *  Bay's eighteen districts read as if it were. They fold onto the five regions
 *  a local browses the Bay by instead: San Francisco, East Bay, South Bay,
 *  North Bay, Peninsula. Still not a compass split — the fold is over the
 *  districts the clustering already found, so the geography is unchanged and
 *  only the buckets are renamed.
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

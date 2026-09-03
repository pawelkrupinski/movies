package models

import tools.Slugify

/** A named sub-region of a [[City]] — London split by compass into Central /
 *  North / East / South / West, a big US metro split into the districts a local
 *  names (Manhattan, Brooklyn, Santa Monica). This is the *identity* of an area
 *  (its label + stable slug); [[CinemaAreaGroup]] pairs it with the cinemas it
 *  holds in a given city.
 *
 *  A city is either **flat** (no areas — the default, `City.areas` empty) or
 *  fully **partitioned** into areas whose union is exactly its `cinemas`.
 *  Clients render one collapsible, individually-(de)selectable group per area.
 *
 *  The label is DATA, not a closed set of cases: London's five compass names are
 *  the named singletons below, but the US ships 105 districts across its five
 *  biggest metros, clustered from its venues' coordinates, so an area has to be
 *  able to carry a name nobody wrote down in advance. The compass singletons keep their exact labels and
 *  slugs — clients persist the slug as a group key (`'areasChosen:' + city`), so
 *  re-slugging an existing area silently forgets a user's choice. */
final case class CinemaArea(label: String, slug: String) {
  /** Whether this area's label names a PLACE somebody could search for — a town
   *  or a district (Manhattan, Santa Monica, Long Island) — as opposed to a
   *  bearing or a catch-all ("Central", "Other areas"), which name nothing
   *  outside the city they sit in. Read by [[City.coveredPlaces]], which puts
   *  the place names into the page's structured data, where a direction would be
   *  worse than nothing: "Central" is a claim about a town called Central. */
  def namesAPlace: Boolean = !CinemaArea.compass.contains(this) && this != CinemaArea.Other
}

object CinemaArea {
  /** An area whose slug is derived from its label — the usual case. The fold is
   *  `Slugify.stable`, the same frozen one that keys title rules: area slugs are
   *  persisted client-side, so they need a fold that never drifts. */
  def apply(label: String): CinemaArea = CinemaArea(label, Slugify.stable(label))

  // London compass areas — `compass`'s order is the client display order.
  val Central: CinemaArea = CinemaArea("Central")
  val North:   CinemaArea = CinemaArea("North")
  val East:    CinemaArea = CinemaArea("East")
  val South:   CinemaArea = CinemaArea("South")
  val West:    CinemaArea = CinemaArea("West")

  /** The five compass areas, in display order. */
  val compass: Seq[CinemaArea] = Seq(Central, North, East, South, West)

  /** The catch-all for venues whose source files them under no sub-region at
   *  all. Nothing uses it today: it held the ~790 US venues that carry no Flicks
   *  `region_slug` until distance clustering gave them the metro nearest their
   *  coordinates instead, which is a real answer where "Other areas" was a
   *  residue. Kept as the honest last resort for a future partitioned city whose
   *  source genuinely cannot place a venue. */
  val Other: CinemaArea = CinemaArea("Other areas")
}

/** A [[CinemaArea]] paired with the cinemas it contains in a particular city —
 *  the unit of `City.areas`, rendered as one collapsible, (de)selectable group. */
final case class CinemaAreaGroup(area: CinemaArea, cinemas: Seq[Cinema]) {
  def cinemaDisplayNames: Seq[String] = cinemas.map(_.displayName)
  lazy val cinemaSet: Set[Cinema]     = cinemas.toSet
}

object CinemaAreaGroup {
  /** Venues grouped by the label each was filed under, biggest group first —
   *  the shape every DATA-DRIVEN split takes (London's compass map is written
   *  out by hand instead). `UsRoster` calls it with each venue's metro label to
   *  split a state into metros, [[UsMetroSubAreas]] with the district label of a
   *  metro too big to browse as one list.
   *
   *  Biggest first: that group is the one most visitors want, and it sinks the
   *  long tail to the bottom where a collapsed group costs nothing. Ties break
   *  on the label so the order is a pure function of the roster.
   *
   *  The slug is derived from the label here rather than carried in the
   *  generated data, so an area a client persists is always keyed by
   *  `Slugify.stable` — the frozen fold — and can never drift from what a
   *  generator happened to emit. */
  def byLabel(venues: Seq[(Cinema, String)]): Seq[CinemaAreaGroup] =
    venues.groupBy { case (_, label) => CinemaArea(label) }.toSeq
      .map { case (area, members) => CinemaAreaGroup(area, members.map(_._1)) }
      .sortBy(g => (-g.cinemas.size, g.area.label))
}

package models

import tools.Slugify

/** A named sub-region of a [[City]] — London split by compass into Central /
 *  North / East / South / West, a US state split by metro area. This is the
 *  *identity* of an area (its label + stable slug); [[CinemaAreaGroup]] pairs it
 *  with the cinemas it holds in a given city.
 *
 *  A city is either **flat** (no areas — the default, `City.areas` empty) or
 *  fully **partitioned** into areas whose union is exactly its `cinemas`.
 *  Clients render one collapsible, individually-(de)selectable group per area.
 *
 *  The label is DATA, not a closed set of cases: London's five compass names are
 *  the named singletons below, but the US ships 470 metros across 55 states,
 *  clustered from its venues' coordinates, so an area has to be able to carry a
 *  name nobody wrote down in advance. The compass singletons keep their exact labels and
 *  slugs — clients persist the slug as a group key (`'areasChosen:' + city`), so
 *  re-slugging an existing area silently forgets a user's choice. */
final case class CinemaArea(label: String, slug: String)

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
  /** Display-name → pill-name for this group's cinemas — the area-scoped
   *  counterpart of `City.cinemaPillMap`, for the `/{city}/{area}/` page's
   *  `_sharedJsConfig`. */
  def cinemaPillMap: Map[String, String] = cinemas.map(c => c.displayName -> c.pillName).toMap
  lazy val cinemaSet: Set[Cinema]        = cinemas.toSet
}

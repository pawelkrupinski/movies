package models

import tools.Slugify

/** A named sub-region of a [[City]] — London split by compass into Central /
 *  North / East / South / West. This is the *identity* of an area
 *  (its label + stable slug); [[CinemaAreaGroup]] pairs it with the cinemas it
 *  holds in a given city.
 *
 *  A city is either **flat** (no areas — the default, `City.areas` empty) or
 *  fully **partitioned** into areas whose union is exactly its `cinemas`.
 *  Clients render one collapsible, individually-(de)selectable group per area.
 *
 *  The compass singletons keep their exact labels and slugs — clients persist the
 *  slug as a group key (`'areasChosen:' + city`), so re-slugging an existing area
 *  silently forgets a user's choice. */
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
}

/** A [[CinemaArea]] paired with the cinemas it contains in a particular city —
 *  the unit of `City.areas`, rendered as one collapsible, (de)selectable group. */
final case class CinemaAreaGroup(area: CinemaArea, cinemas: Seq[Cinema]) {
  def cinemaDisplayNames: Seq[String] = cinemas.map(_.displayName)
  lazy val cinemaSet: Set[Cinema]     = cinemas.toSet
}

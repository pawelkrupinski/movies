package models

import java.util.Locale

/** A named sub-region of a [[City]] — e.g. London split by compass into
 *  Central / North / East / South / West. This enum is the *identity* of an
 *  area (its label + stable slug); [[CinemaAreaGroup]] pairs it with the
 *  cinemas it holds in a given city.
 *
 *  A city is either **flat** (no areas — the default, `City.areas` empty) or
 *  fully **partitioned** into areas whose union is exactly its `cinemas`. The
 *  enum is the general, cross-city set of area names: a new split city adds its
 *  own cases here. Clients render one collapsible, individually-(de)selectable
 *  group per area. */
enum CinemaArea(val label: String):
  // London compass areas — declaration order is the client display order.
  case Central extends CinemaArea("Central")
  case North   extends CinemaArea("North")
  case East    extends CinemaArea("East")
  case South   extends CinemaArea("South")
  case West    extends CinemaArea("West")

  /** Stable kebab-case id — the group key the clients persist and address by
   *  (`Central` → `"central"`). */
  lazy val slug: String =
    label.toLowerCase(Locale.ROOT).replaceAll("[^a-z0-9]+", "-").replaceAll("(^-|-$)", "")

/** A [[CinemaArea]] paired with the cinemas it contains in a particular city —
 *  the unit of `City.areas`, rendered as one collapsible, (de)selectable group. */
final case class CinemaAreaGroup(area: CinemaArea, cinemas: Seq[Cinema]):
  def cinemaDisplayNames: Seq[String] = cinemas.map(_.displayName)

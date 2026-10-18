package models

import java.text.Collator
import java.util.Locale

/** Shared city-list rendering used by both the global [[City]] view and each
 *  per-country [[Country]] view, so collation + JSON shape live in one place. */
private[models] object CityListing {
  /** `cities` alphabetically by their names, under `locale`'s collation. */
  def sorted(cities: Seq[City], locale: Locale): Seq[City] = sortedLabels(cities, locale)(_.labels.nominative)

  /** `items` alphabetically by `label`, under `locale`'s collation — [[sorted]]'s
   *  ordering, and the one for the HEADINGS of a grouped picker, which are place
   *  names too but not [[City]]s.
   *
   *  A code-point sort is not the alphabet anywhere the names leave ASCII: it
   *  files Köln after Krefeld (ö is code point 246, past every letter) and Łódź
   *  past Z, which is exactly the ordering a reader scanning for a name cannot
   *  use. */
  def sortedLabels[A](items: Seq[A], locale: Locale)(label: A => String): Seq[A] = {
    val collator = Collator.getInstance(locale)
    items.sortWith((a, b) => collator.compare(label(a), label(b)) < 0)
  }

  /** One alphabetical [[CityGroup]] per `groupOf` value over `cities`, each holding
   *  its cities alphabetically, the groups ordered by the name each SHOWS
   *  ([[CityGroup.displayLabel]]) — a group holding one page shows that page, and
   *  sorting on the group's own label would misplace it. The shape of Poland's
   *  voivodeships, Germany's Bundesländer and Spain's provinces. */
  def groupedAlphabetically[C <: City](cities: Seq[C], locale: Locale)(groupOf: C => String): Seq[CityGroup] = {
    val built = cities.groupBy(groupOf).toSeq.map { case (name, members) =>
      CityGroup(name, tools.Slugify.stable(name), sorted(members, locale))
    }
    sortedLabels(built, locale)(_.displayLabel)
  }

  def json(cities: Seq[City]): String =
    cities
      .map(c => s"""{"slug":"${c.slug}","name":"${c.labels.nominative}","lat":${c.lat},"lon":${c.lon}}""")
      .mkString("[", ",", "]")
}

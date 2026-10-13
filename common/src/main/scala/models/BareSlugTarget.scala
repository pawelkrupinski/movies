package models

/** Where a link that names no country goes ([[Country.bareSlugTarget]]): the
 *  country, its city, and whether [[LegacySlugOwners]] changed the answer — sent it
 *  somewhere other than the one country that has the slug, or the first in order. */
final case class BareSlugTarget(country: Country, city: City, tableChangedAnswer: Boolean)

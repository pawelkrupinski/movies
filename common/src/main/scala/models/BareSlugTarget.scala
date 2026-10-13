package models

/** Where a link that names no country goes ([[Country.bareSlugTarget]]): the
 *  country, its city, and whether [[LegacySlugOwners]] decided it. */
final case class BareSlugTarget(country: Country, city: City, viaLegacyOwner: Boolean)

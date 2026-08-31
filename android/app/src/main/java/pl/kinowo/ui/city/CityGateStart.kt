package pl.kinowo.ui.city

/**
 * What the first-launch city gate should do when it opens.
 *
 * [countryCode] is the country every search and list is scoped to. [locate] is
 * false when the user has just picked a country themselves: the gate then goes
 * straight to that country's city list, because offering a located city would
 * answer "show me Germany" with "you're near Poznan". It stays true for the
 * first launch, where no choice has been expressed and the nearest city is the
 * most useful thing to offer.
 *
 * Absence of the whole value (a null [CityGateStart]) means the stored choices
 * have not been read yet — the gate waits rather than assuming either field.
 */
data class CityGateStart(val countryCode: String, val locate: Boolean)

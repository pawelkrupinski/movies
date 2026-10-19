package tools

import java.util.Locale

/** Does a credited "director" name a company rather than a person? A stage relay's film record often credits its
  * house — "The Metropolitan Opera", "National Theatre Live", "Royal Opera House", "Berliner Philharmoniker" — where
  * a venue credits the stage director, so the two are no contradiction. Judged by a word only a company's name
  * carries, in the languages the venues bill in. */
object OrganisationName {
  private val Words = Set(
    "opera", "oper", "opéra", "ópera", "opery", "operahouse",
    "theatre", "theater", "théâtre", "teatro", "teatr", "theatres",
    "ballet", "balet", "ballett", "philharmonic", "philharmoniker", "philharmonie", "filharmonia", "filarmonica", "filarmónica",
    "orchestra", "orchester", "orchestre", "orquesta", "orkiestra", "symphony", "sinfonie", "symfonia",
    "company", "ensemble", "choir", "chor", "festival", "house", "live", "productions", "studios", "pictures", "entertainment")

  def apply(name: String): Boolean =
    name.toLowerCase(Locale.ROOT).split("[^\\p{L}]+").exists(Words.contains)
}

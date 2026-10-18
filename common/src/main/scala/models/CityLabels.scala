package models

import java.util.Locale

/** The Polish name of a city in the grammatical forms the templates need:
 *  nominative ("Poznań"), genitive plural for "…skich kin" ("poznańskich"),
 *  and locative for "w …" ("Poznaniu"). Kept as data so no template hardcodes
 *  a city name. */
final case class CityLabels(nominative: String, genitivePlural: String, locative: String)

object CityLabels {
  /** The labels of a place in a language that does not decline its name (English,
   *  German, Spanish): every slot carries the name as it is. */
  def undeclined(name: String): CityLabels = CityLabels(name, name, name)
}

/** Per-language rendering of a city's [[CityLabels]] into the grammatical
 *  phrases the UI needs. Polish declines the name (locative "w Poznaniu" /
 *  "we Wrocławiu", genitive-plural adjective "poznańskich"); languages that
 *  don't decline (English, and the default fallback) read off the plain
 *  nominative ("in London", "London"). Selected by the city's country language
 *  so a per-country deployment renders naturally without any name hardcoded. */
private[models] sealed trait CityGrammar {
  def locativePhrase(labels: CityLabels): String
  def genitivePluralLabel(labels: CityLabels): String
}

private[models] object CityGrammar {

  /** Polish: the declined forms from [[CityLabels]], byte-identical to what the
   *  templates/OG-card generator emitted before i18n. "we" replaces "w" before a
   *  word starting with W/F + consonant (the awkward "w w-" / "w f-" cluster). */
  private object Polish extends CityGrammar {
    def locativePhrase(labels: CityLabels): String = {
      val loc    = labels.locative
      val vowels = "aeiouyąęó"
      val we     = loc.length >= 2 && (loc(0) == 'W' || loc(0) == 'F') &&
                   !vowels.contains(loc(1).toLower)
      s"${if (we) "we" else "w"} $loc"
    }
    def genitivePluralLabel(labels: CityLabels): String = labels.genitivePlural
  }

  /** Non-declining languages: the nominative, with the language's own
   *  preposition in front of it for the locative slot ("in London", "en Madrid").
   *  The preposition is a parameter rather than a literal because it is the only
   *  thing that differs — German's is "in" like English's, Spanish's is not, and
   *  a hardcoded "in" would have put "in Madrid" in every Spanish share card and
   *  every piece of Spanish structured data. */
  private final class Nominative(preposition: String) extends CityGrammar {
    def locativePhrase(labels: CityLabels): String      = s"$preposition ${labels.nominative}"
    def genitivePluralLabel(labels: CityLabels): String = labels.nominative
  }

  private val In = new Nominative("in")   // English, German
  private val En = new Nominative("en")   // Spanish

  def of(locale: Locale): CityGrammar = locale.getLanguage match {
    case "pl" => Polish
    case "es" => En
    case _    => In
  }
}

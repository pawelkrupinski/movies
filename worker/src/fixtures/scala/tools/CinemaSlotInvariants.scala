package tools

import models.SourceData
import services.cinemas.CountryNames
import services.movies.FilmRuntime

/** The shape every field of a cinema slot must have: what `CinemaSlotInvariantsSpec` holds every corpus to, and what
 *  `scripts/title-trace` says one raw title's slot would raise. */
object CinemaSlotInvariants {

  private val Runtime        = """(?i)^\d{1,3}\s*(?:['’′]|min\.?|minut\w*|mins?)$""".r
  private val DateOrNumber   = """^[\d\s./:-]+$""".r
  /** A title ending on a separator or a conjunction / preposition. Polish "i" only lower-case: a capital "I" is a
   *  numeral or a pronoun ("Part I", "Withnail & I", "zestaw bajek I"). */
  private val DanglingEnd    = """(?u)(?:[,;:/–—-]|\s(?i:oraz|and|und|y|et|w|z|of|the|a)|\si)$""".r
  /** A person carrying the next credit's label ("Maciej Kawalski aktorzy: Marcin Dorociński") or a production
   *  line's year ("Maria Dębska Polska 2026"): the credits line read whole into one field. */
  private val CreditLabel    =
    """(?iu)(?<!\p{L})(?:aktorzy|obsada|występują|scenariusz|cast|starring|darsteller|besetzung|reparto)\s*:""".r
  private val YearInName     = """(?<!\d)(?:18|19|20)\d{2}(?!\d)""".r
  /** A credits block's opening label, in each served country's language. */
  private val CreditsOpening =
    ("""(?iu)^(?:obsada|reżyseria|reż\.|scenariusz|produkcja|cast|director|directed by|starring|""" +
      """regie|darsteller|besetzung|dirección|dirigida por|reparto|intérpretes)\s*[:.]""").r
  /** The corpus was captured in 2026: a release year past this is a misread number, not a film. */
  private val LatestYear     = 2030

  /** Each shape rule `slot` breaks, as `rule: value`. */
  def violations(slot: SourceData): Seq[String] = {
    val people = (slot.director ++ slot.cast).map(_.trim).filter(_.nonEmpty).flatMap { person =>
      if (Runtime.matches(person)) Some(s"person-is-runtime: $person")
      else if (DateOrNumber.matches(person)) Some(s"person-is-date-or-number: $person")
      else if (CountryNames.isCountry(person)) Some(s"person-is-country: $person")
      else if (CreditLabel.findFirstIn(person).isDefined) Some(s"person-carries-credit-label: $person")
      else Option.when(YearInName.findFirstIn(person).isDefined)(s"person-carries-year: $person")
    }
    val title = slot.title.map(_.trim).filter(t => DanglingEnd.findFirstIn(t).isDefined).map(t => s"title-ends-mid-phrase: $t")
    val runtime = slot.runtimeMinutes.filterNot(FilmRuntime.plausible).map(m => s"runtime-implausible: $m")
    val year = slot.releaseYear.filterNot(y => y >= 1880 && y <= LatestYear).map(y => s"year-implausible: $y")
    val synopsis = slot.synopsis.map(_.trim).filter(CreditsOpening.findFirstIn(_).isDefined).map(s => s"synopsis-is-credits: $s")
    people ++ title ++ runtime ++ year ++ synopsis
  }
}

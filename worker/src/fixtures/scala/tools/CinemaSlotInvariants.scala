package tools

import models.SourceData

import scala.annotation.nowarn

/** The shape every field of a cinema slot must have: what `CinemaSlotInvariantsSpec` holds every corpus to, and what
 *  `scripts/title-trace` says one raw title's slot would raise. */
object CinemaSlotInvariants {

  /** [[models.FilmRuntime.plausible]], reached in either package a ref keeps it (services.movies before 2026-11-19,
   *  models since): scripts/title-trace drops this file into refs from before the move. */
  @nowarn("msg=unused import")
  private def plausibleRuntime(minutes: Int): Boolean = {
    import services.movies.*, models.*
    FilmRuntime.plausible(minutes)
  }

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

  /** `CountryNames.isCountry`, reached by name: this title-trace tool file also compiles into refs older than
   *  CountryNames' move from services.cinemas to models (scripts/identity-slice-old-ref-test.sh), so it names neither. */
  private val isCountry: String => Boolean = {
    val countryNames = Seq("models.CountryNames$", "services.cinemas.CountryNames$").iterator
      .flatMap(name => scala.util.Try(Class.forName(name).getField("MODULE$").get(null)).toOption).next()
    val method = countryNames.getClass.getMethod("isCountry", classOf[String])
    name => method.invoke(countryNames, name).asInstanceOf[Boolean]
  }

  /** Each shape rule `slot` breaks, as `rule: value`. */
  def violations(slot: SourceData): Seq[String] = {
    val people = (slot.director ++ slot.cast).map(_.trim).filter(_.nonEmpty).flatMap { person =>
      if (Runtime.matches(person)) Some(s"person-is-runtime: $person")
      else if (DateOrNumber.matches(person)) Some(s"person-is-date-or-number: $person")
      else if (isCountry(person)) Some(s"person-is-country: $person")
      else if (CreditLabel.findFirstIn(person).isDefined) Some(s"person-carries-credit-label: $person")
      else Option.when(YearInName.findFirstIn(person).isDefined)(s"person-carries-year: $person")
    }
    val title = slot.title.map(_.trim).filter(t => DanglingEnd.findFirstIn(t).isDefined).map(t => s"title-ends-mid-phrase: $t")
    val runtime = slot.runtimeMinutes.filterNot(plausibleRuntime).map(m => s"runtime-implausible: $m")
    val year = slot.releaseYear.filterNot(y => y >= 1880 && y <= LatestYear).map(y => s"year-implausible: $y")
    val synopsis = slot.synopsis.map(_.trim).filter(CreditsOpening.findFirstIn(_).isDefined).map(s => s"synopsis-is-credits: $s")
    people ++ title ++ runtime ++ year ++ synopsis
  }
}

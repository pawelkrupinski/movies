package services.movies

import models.SourceData
import services.cinemas.CountryNames

/**
 * What every cinema client's parser hands the pipeline is SANE field by field, over every corpus
 * ([[CorpusShapeSpec]]): each cinema slot of every film the Polish fixture boot built, and each
 * listing of every country's recorded corpus built into its slot.
 *
 * The class of failure: a parser reading a field off the wrong node, line or delimiter. Kino Muza
 * stored "75’" and "USA" as directors (the first line, not the "reż." one); Elektrownia cut
 * "Powiedz mi, co czujesz" at its comma; Ekobilet took a metadata paragraph for the synopsis;
 * Amondo pasted another film's details. Each passed its own client spec, written against the
 * film its author looked at. The checks here hold every film of every client to the shape a
 * field must have — a director is a person, not a runtime, a country or a date; a title does not
 * end mid-phrase; a runtime and a year are plausible; a synopsis is prose, not a credit line —
 * and name the cinema and film that breaks one.
 */
class CinemaSlotInvariantsSpec extends CorpusShapeSpec {

  import CinemaSlotInvariantsSpec._

  protected def rule = "hold every cinema slot's fields to their shapes"

  protected def findings(corpus: ListingCorpus, listing: CorpusListing): Seq[String] = violations(listing.slot)

  protected def remedy =
    "These cinema slots carry a field read off the wrong node, line or delimiter. Fix the client's parser, or " +
      "allowlist the listing with why the value is right:"

  private val RecordedBeforeFix =
    "recorded before the parser fix named here; the recorded corpus is the parser's OUTPUT, so it keeps the old value " +
      "until re-recorded — listed in awaitingReRecord, so a fresh recording reports it instead of failing; drop it once every corpus is re-recorded"

  /** Listings a parser fix has already cured, whose recorded slot still carries the old value. */
  private val recordedBeforeFix: Map[AllowedListing, String] =
    Map(AllowedListing("Kino Mikro", "Zaproszenie") -> (
      s"director = the whole credits line, 2026-10-03: $RecordedBeforeFix (SystemBiletowy's 'aktorzy' label) — every recording " +
        "since carries it clean, but the checked-in PL corpus (test/resources/fixtures/corpus, 2026-07-29) still has the old value"),
    AllowedListing("Phoenix Cinema East Finchley", "Tenzing") ->
      s"cast '2026', a year typed into the cast field, 2026-10-13 (run 38326582997): $RecordedBeforeFix (SavoyClient.Credits reads a bare year as the film's)",
    AllowedListing("Dundee Contemporary Arts (DCA)", "The Everest Dream + Q&A") ->
      s"a facts sheet heading the description, 2026-10-13 (run 38326582997): $RecordedBeforeFix (SpektrixClient reads the synopsis past it)",
    AllowedListing("Teatr Ziemi Rybnickiej", "Trzy pożegnania") -> (
      s"the 'Scenariusz:' credit as the synopsis, 2026-10-14 (run 38498856157): $RecordedBeforeFix (TeatrZiemiRybnickiejClient " +
        "no longer reads the 'zł' of 'złe samopoczucie' as a price row, and reads 'Scenariusz' as a credit label)")) ++
    Seq("Bad Apples", "Derek vs Derek", "FFS-Everybody Digs Bill Evans", "FFS-Everybody to Kenmure Street", "FFS-Kontinental 25",
      "FFS-Resurrection", "FFS-Two Prosecutors", "Practical Magic 2", "Pressure", "Sense and Sensibility.",
      "Shaun The Sheep: The Beast of Mossy Bottom").map { listing =>
      AllowedListing("Theatr Gwaun Fishguard", listing) ->
        s"a 'Director: …/UK/2026/88’' credit line heading the synopsis, 2026-10-13 (run 38326582997): $RecordedBeforeFix (SavoyClient.CreditLine reads it into the film's record)"
    }

  override protected val awaitingReRecord: Set[AllowedListing] = recordedBeforeFix.keySet

  protected val allowlist: Map[AllowedListing, String] = Map(
    AllowedListing("Pictureville (Science and Media Museum Bradford)", "Jubilee (1978)") ->
      "'Jordan' is the punk icon Pamela Rooke's screen name, billed so in Jarman's Jubilee — a person, not the country",
    AllowedListing("Kino 1410", "Windą na szafot | Klasyka w") ->
      "the venue's own feed truncated the title (no rule or parser cuts 'kinie'); the listing has since left the feed, so it cannot be re-checked",
    AllowedListing.anywhere("Umamusume: Pretty Derby - Beginning of a") ->
      "Cinema City's feed cuts long titles (as it cut 'skarpetek 3. Ale ko'): the source's own spelling, nothing to read better",
    AllowedListing.anywhere("Lieblingsmenschen - Die außergewöhnliche  Freundschaft von A") -> (
      "the venue's box office cuts names at 60 characters ('…von Agnes und Amir'), and Cineamo serves only that name and its English " +
        "record ('Agnes & Amir', kept as the original title): the source's own spelling, nothing to read better"),
    AllowedListing.anywhere("Timo Wopp LIVE: Work Hard. Play Hard. BEST OF") ->
      "the comedian's tour is called 'Work Hard. Play Hard. Best Of' — its title does end on 'of'",
    AllowedListing("Kino Rialto", "EKIPA ZWIERZAKÓW") -> (
      "TODO(parser): the checked-in 2026-07-29 corpus has Rialto's 'Reż.' line glued to its production line (director " +
        "'Caroline Origer Belgia', 'Francja', … '85 minut'); the event page has left the programme (not in the 2026-10-04 " +
        "recording), so the parser cannot be re-checked against it — parked"),
  ) ++ recordedBeforeFix

  "the invariants" should "catch each wrong-node read the clients shipped" in {
    violations(SourceData(director = Seq("75’"))) shouldBe Seq("person-is-runtime: 75’")
    violations(SourceData(director = Seq("USA"))) shouldBe Seq("person-is-country: USA")
    violations(SourceData(director = Seq("Vereinigte Staaten"))) shouldBe Seq("person-is-country: Vereinigte Staaten")
    violations(SourceData(cast = Seq("Estados Unidos"))) shouldBe Seq("person-is-country: Estados Unidos")
    violations(SourceData(cast = Seq("12.06"))) shouldBe Seq("person-is-date-or-number: 12.06")
    violations(SourceData(director = Seq("Maciej Kawalski aktorzy: Marcin Dorociński", "Maria Dębska Polska 2026"))) shouldBe
      Seq("person-carries-credit-label: Maciej Kawalski aktorzy: Marcin Dorociński", "person-carries-year: Maria Dębska Polska 2026")
    violations(SourceData(title = Some("Powiedz mi,"))) shouldBe Seq("title-ends-mid-phrase: Powiedz mi,")
    violations(SourceData(title = Some("Duch i"))) shouldBe Seq("title-ends-mid-phrase: Duch i")
    violations(SourceData(runtimeMinutes = Some(0))) shouldBe Seq("runtime-implausible: 0")
    violations(SourceData(runtimeMinutes = Some(1250))) shouldBe Seq("runtime-implausible: 1250")
    violations(SourceData(runtimeMinutes = Some(6000))) shouldBe Seq("runtime-implausible: 6000")
    violations(SourceData(releaseYear = Some(26))) shouldBe Seq("year-implausible: 26")
    violations(SourceData(synopsis = Some("Reżyseria: Jan Kowalski, obsada: Anna Nowak"))) shouldBe
      Seq("synopsis-is-credits: Reżyseria: Jan Kowalski, obsada: Anna Nowak")
    violations(SourceData(synopsis = Some("Regie: Tom Tykwer. Darsteller: Lars Eidinger"))) shouldBe
      Seq("synopsis-is-credits: Regie: Tom Tykwer. Darsteller: Lars Eidinger")
    violations(SourceData(synopsis = Some("Reparto: Penélope Cruz"))) shouldBe Seq("synopsis-is-credits: Reparto: Penélope Cruz")
    violations(SourceData(title = Some("Diuna: Część druga"), director = Seq("Denis Villeneuve"),
      cast = Seq("Timothée Chalamet"), runtimeMinutes = Some(166), releaseYear = Some(2024),
      synopsis = Some("Paul Atryda jednoczy się z Chani i Fremenami."))) shouldBe empty
    violations(SourceData(title = Some("Kler"), runtimeMinutes = Some(3))) shouldBe empty
    violations(SourceData(title = Some("Withnail & I"))) shouldBe empty
    violations(SourceData(title = Some("Harry Potter and the Deathly Hallows: Part I"))) shouldBe empty
  }
}

object CinemaSlotInvariantsSpec {

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

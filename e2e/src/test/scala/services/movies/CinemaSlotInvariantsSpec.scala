package services.movies

import models.{Cinema, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.CountryNames
import tools.FixtureTestWiring

/**
 * What every cinema client's parser hands the pipeline is SANE field by field, over the whole
 * recorded corpus: each cinema slot of every film the fixture boot built.
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
class CinemaSlotInvariantsSpec extends AnyFlatSpec with Matchers {

  import CinemaSlotInvariantsSpec._

  /** `cinema slug: rule: value` → why that slot may break the rule. */
  private val Allowlist: Map[String, String] = Map()

  private lazy val wiring: FixtureTestWiring = {
    val w = new FixtureTestWiring("08-06-2026")
    w.bootStartup()
    w
  }

  "the invariants" should "catch each wrong-node read the clients shipped" in {
    violations(SourceData(director = Seq("75’"))) shouldBe Seq("person-is-runtime: 75’")
    violations(SourceData(director = Seq("USA"))) shouldBe Seq("person-is-country: USA")
    violations(SourceData(cast = Seq("12.06"))) shouldBe Seq("person-is-date-or-number: 12.06")
    violations(SourceData(title = Some("Powiedz mi,"))) shouldBe Seq("title-ends-mid-phrase: Powiedz mi,")
    violations(SourceData(title = Some("Duch i"))) shouldBe Seq("title-ends-mid-phrase: Duch i")
    violations(SourceData(runtimeMinutes = Some(0))) shouldBe Seq("runtime-implausible: 0")
    violations(SourceData(runtimeMinutes = Some(1250))) shouldBe Seq("runtime-implausible: 1250")
    violations(SourceData(releaseYear = Some(26))) shouldBe Seq("year-implausible: 26")
    violations(SourceData(synopsis = Some("Reżyseria: Jan Kowalski, obsada: Anna Nowak"))) shouldBe
      Seq("synopsis-is-credits: Reżyseria: Jan Kowalski, obsada: Anna Nowak")
    violations(SourceData(title = Some("Diuna: Część druga"), director = Seq("Denis Villeneuve"),
      cast = Seq("Timothée Chalamet"), runtimeMinutes = Some(166), releaseYear = Some(2024),
      synopsis = Some("Paul Atryda jednoczy się z Chani i Fremenami."))) shouldBe empty
    violations(SourceData(title = Some("Kler"), runtimeMinutes = Some(3))) shouldBe empty
  }

  "every cinema slot the corpus builds" should "hold every field to its shape" in {
    val slots: Seq[(Cinema, SourceData)] =
      ScheduleCorpusText.recordsByFilmId(wiring).values.toSeq.distinct.flatMap(_.cinemaData.toSeq)
    slots.size should be > 500
    val found = slots.flatMap { case (cinema, slot) =>
      violations(slot).map(v => s"${cinema.slug}: $v").filterNot(Allowlist.contains)
        .map(v => s"$v   (film '${slot.rawTitle.orElse(slot.title).getOrElse("?")}')")
    }.distinct.sorted
    withClue("These cinema slots carry a field read off the wrong node, line or delimiter. Fix the client's " +
      "parser, or allowlist the slot with why the value is right:\n" + found.mkString("\n") + "\n")(found shouldBe empty)
  }

  it should "keep every allowlist entry still breaking its rule (the backlog only shrinks)" in {
    val live = ScheduleCorpusText.recordsByFilmId(wiring).values.toSeq.flatMap(_.cinemaData.toSeq)
      .flatMap { case (cinema, slot) => violations(slot).map(v => s"${cinema.slug}: $v") }.toSet
    withClue("Allowlisted but no longer broken — drop the entry: ")((Allowlist.keySet -- live).toSeq.sorted shouldBe empty)
  }
}

object CinemaSlotInvariantsSpec {

  private val Runtime        = """(?i)^\d{1,3}\s*(?:['’′]|min\.?|minut\w*|mins?)$""".r
  private val DateOrNumber   = """^[\d\s./:-]+$""".r
  private val DanglingEnd    = """(?iu)(?:[,;:/–—-]|\s(?:i|oraz|and|und|y|et|w|z|of|the|a))$""".r
  private val CreditsOpening =
    """(?iu)^(?:obsada|reżyseria|reż\.|scenariusz|produkcja|cast|director|directed by|starring|regie|dirección)\s*[:.]""".r
  /** The corpus was captured in 2026: a release year past this is a misread number, not a film. */
  private val LatestYear     = 2030

  /** Each shape rule `slot` breaks, as `rule: value`. */
  def violations(slot: SourceData): Seq[String] = {
    val people = (slot.director ++ slot.cast).map(_.trim).filter(_.nonEmpty).flatMap { person =>
      if (Runtime.matches(person)) Some(s"person-is-runtime: $person")
      else if (DateOrNumber.matches(person)) Some(s"person-is-date-or-number: $person")
      else Option.when(CountryNames.isPolish(person))(s"person-is-country: $person")
    }
    val title = slot.title.map(_.trim).filter(t => DanglingEnd.findFirstIn(t).isDefined).map(t => s"title-ends-mid-phrase: $t")
    val runtime = slot.runtimeMinutes.filterNot(m => m >= 1 && m <= 900).map(m => s"runtime-implausible: $m")
    val year = slot.releaseYear.filterNot(y => y >= 1880 && y <= LatestYear).map(y => s"year-implausible: $y")
    val synopsis = slot.synopsis.map(_.trim).filter(CreditsOpening.findFirstIn(_).isDefined).map(s => s"synopsis-is-credits: $s")
    people ++ title ++ runtime ++ year ++ synopsis
  }
}

package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}

/**
 * A scraper reads the year of a page date the page prints without one ONLY through
 * `ScraperParse.upcomingDate` / `upcomingMonthDate` (with `ScraperParse.monthDay` for a numeric
 * day and month) — never a year rule of its own.
 *
 * The class of failure: a dozen clients each carried their own "this year, or next if it's past"
 * rule, and each got a year boundary wrong in its own way — a 29 lutego listed in a non-leap year
 * built `LocalDate.of(thisYear, 2, 29)` and was dropped (The Old Court, BOK, Kino Patria, …); a
 * late-December row still on the page in early January was put eleven months into the future
 * (Kino Muza, Kino IKM, Awangarda 2, Promień). The shared rule handles both, and lets each page
 * pick only its grace. Rules, each naming file:line, in the cinema clients:
 *
 *  1. No year taken off `today` (`today.getYear`) — a yearless date's year comes from the shared
 *     rule.
 *  2. No "roll it forward a year when it looks past" (`if (x.isBefore(today…)) x.plusYears(1)`).
 */
class NoHandRolledYearInferenceSpec extends AnyFlatSpec with Matchers {

  import ScalaSourceScan.{codeOf, read, scalaFiles}

  private val ClientRoot = "worker/src/main/scala/services/cinemas"

  /** file → why it may still derive a year itself. */
  private val Allowlist: Map[String, String] = Map(
    s"$ClientRoot/common/ScraperParse.scala" -> "the shared year rule itself",
    s"$ClientRoot/pl/KinoZamekClient.scala" -> (
      "a cycle page lists the whole season, so a yearless date is the occurrence NEAREST today (June's opening night " +
        "read in August is past, not eleven months ahead) — the opposite of upcomingDate's next-occurrence rule"),
    s"$ClientRoot/pl/KinoPortClient.scala" -> (
      "walks one post's day headers in order and rolls the year when the month goes backwards, re-anchored by any " +
        "month header that prints a year: the post's own order decides, not today"),
    s"$ClientRoot/pl/KinoDKFRumcajsClient.scala" ->
      "a monthly post whose title prints its year; today's year is only the fallback for a title without one",
    s"$ClientRoot/pl/MuranowClient.scala" ->
      "the calendar header prints the month's year; today's year is only the fallback for a header without one",
  )

  private val YearOffToday = """\b\w*[Tt]oday\w*\.getYear\b""".r
  private val RollForward  = """\.isBefore\([^\n]*\b\w*[Tt]oday\b[^\n]*\.plusYears\(1\)""".r

  private[tools] def yearRules(src: String): Seq[Int] =
    Seq(YearOffToday, RollForward)
      .flatMap(_.findAllMatchIn(src).map(m => src.substring(0, m.start).count(_ == '\n') + 1))
      .distinct.sorted

  private lazy val sources = scalaFiles(Seq(ClientRoot)).map(p => p.toString -> codeOf(p))

  "the year-rule matchers" should "catch each hand-rolled year rule the clients carried" in {
    yearRules("val year = if (month < today.getMonthValue) today.getYear + 1 else today.getYear") shouldBe Seq(1)
    yearRules("val candidate = LocalDate.of(today.getYear, month, day)") shouldBe Seq(1)
    yearRules("if (candidate.isBefore(today.minusMonths(6))) candidate.plusYears(1) else candidate") shouldBe Seq(1)
    yearRules("if (thisYear.isBefore(today.minusWeeks(1))) thisYear.plusYears(1) else thisYear") shouldBe Seq(1)
    yearRules("ScraperParse.monthDay(day, month).flatMap(ScraperParse.upcomingDate(_, today, Period.ofMonths(6)))") shouldBe empty
    yearRules("screeningsUrl(cinemaId, today.atStartOfDay, today.plusYears(1).atStartOfDay)") shouldBe empty
    yearRules("s\"date=${ym.getYear}-${ym.getMonthValue}\"") shouldBe empty
  }

  "the cinema clients" should "read a yearless page date's year only through ScraperParse.upcomingDate" in {
    sources.size should be > 100
    val found = sources.filterNot { case (path, _) => Allowlist.contains(path) }.flatMap { case (path, src) =>
      yearRules(src).map(line => s"$path:$line: ${read(Paths.get(path)).linesIterator.drop(line - 1).next().trim}")
    }
    withClue("Read the date's year through ScraperParse.monthDay(day, month).flatMap(ScraperParse.upcomingDate(_, today, " +
      "grace)) — or upcomingMonthDate for a calendar listing only the current month onwards — instead of a year " +
      "rule of the client's own; or add the file to Allowlist with why its year cannot come from today:\n" +
      found.mkString("\n") + "\n")(found shouldBe empty)
  }

  it should "keep every allowlist entry still deriving a year itself (the backlog only shrinks)" in {
    val stale = Allowlist.keys.toSeq.sorted.filterNot { file =>
      Files.exists(Paths.get(file)) && yearRules(codeOf(Paths.get(file))).nonEmpty
    }
    withClue("Allowlisted but no longer deriving a year — drop the entry:\n" + stale.mkString("\n") + "\n")(stale shouldBe empty)
  }
}

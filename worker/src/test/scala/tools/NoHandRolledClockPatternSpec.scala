package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Paths

/**
 * A cinema client spells a showtime's clock in its patterns ONLY through `ScraperParse`'s clock
 * fragments (`ClockParts`, `ClockPartsDotted`, `ClockText`, `ClockTextDotted`, `IsoClockText`,
 * composed with `raw"""…${ScraperParse.ClockParts}…"""`), and reads a captured hour and minute
 * only through `ScraperParse.clockAt` / `parseHHmm`.
 *
 * Forty clients each carried a private copy of the same `(\d{1,2}):(\d{2})` and the
 * `Try(LocalTime.of(m.group(1).toInt, m.group(2).toInt))` that reads it — the repo's extract-at-two-uses
 * rule forty times over, and the shape the yearless-date rule drifted from (one copy each, each
 * wrong at a different edge). Rules, each naming file:line, in the cinema clients:
 *
 *  1. No clock regex literal: `\d{1,2}:\d{2}`, `(\d{1,2}):(\d{2})`, `\d{2}:\d{2}`, or the dotted
 *     `[.:]` / `[:.]` forms.
 *  2. No `LocalTime.of(m.group(…)…)` — `ScraperParse.clockAt(m, hourGroup)` reads it without a
 *     throw that would take the page's other screenings with it.
 */
class NoHandRolledClockPatternSpec extends AnyFlatSpec with Matchers {

  import ScalaSourceScan.{codeOf, read, scalaFiles}

  private val ClientRoot = "worker/src/main/scala/services/cinemas"
  private val Source     = s"$ClientRoot/common/ScraperParse.scala"

  private val ClockLiteral =
    """\\d\{(?:1,2|2)\}\)?(?::|\[[.:]{2}\])\(?\\d\{2\}""".r
  private val GroupClock = """LocalTime\.of\(\s*\w+\.group\(""".r

  private[tools] def clockRules(src: String): Seq[Int] =
    Seq(ClockLiteral, GroupClock)
      .flatMap(_.findAllMatchIn(src).map(m => src.substring(0, m.start).count(_ == '\n') + 1))
      .distinct.sorted

  "the clock matchers" should "catch each private clock pattern the clients carried" in {
    clockRules("""(\d{1,2}):(\d{2})""") shouldBe Seq(1)
    clockRules("""godz\.?\s*(\d{1,2}:\d{2})""") shouldBe Seq(1)
    clockRules("""- (\d{4}-\d{2}-\d{2}) (\d{2}:\d{2}) -""") shouldBe Seq(1)
    clockRules("""\b(\d{1,2})[.:](\d{2})\b""") shouldBe Seq(1)
    clockRules("""^\d{1,2}[:.]\d{2}\s*-""") shouldBe Seq(1)
    clockRules("Try(LocalTime.of(m.group(1).toInt, m.group(2).toInt)).toOption") shouldBe Seq(1)
    clockRules("""godz\.?\s*(${ScraperParse.ClockText})""") shouldBe empty
    clockRules("""(\d{1,2})\.(\d{1,2})\.(\d{4})""") shouldBe empty   // a date
    clockRules("ScraperParse.clockAt(m, 1)") shouldBe empty
  }

  "the cinema clients" should "spell and read a clock only through ScraperParse" in {
    val sources = scalaFiles(Seq(ClientRoot)).filterNot(_.toString == Source)
    sources.size should be > 100
    val found = sources.flatMap { path =>
      clockRules(codeOf(path)).map(line => s"$path:$line: ${read(Paths.get(path.toString)).linesIterator.drop(line - 1).next().trim}")
    }
    withClue("Compose the clock from ScraperParse's fragments (raw\"\"\"…${ScraperParse.ClockParts}…\"\"\") and read it with " +
      "ScraperParse.clockAt(m, hourGroup) or parseHHmm, instead of a pattern of the client's own:\n" +
      found.mkString("\n") + "\n")(found shouldBe empty)
  }
}

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
 *     `[.:]` / `[:.]` / `\.` forms (a `\d{2}\.\d{2}` followed by a further `\.` field is a date).
 *  2. No clock built from parsed numbers: `LocalTime.of(h, m)` / `date.atTime(h, m)` with a first
 *     argument that is not a literal — `ScraperParse.clock` / `clockAt` / `meridiemClockAt` read it
 *     without a throw that would take the page's other screenings with it. The first cut matched
 *     only `LocalTime.of(m.group(`, and nine reads (a `split(":")`, an extractor's bound `h`, a
 *     `date.atTime(m.group(4).toInt, …)`, two hand-rolled am/pm converters, a hand-rolled
 *     `hh < 24 && mm < 60`) went on unseen.
 */
class NoHandRolledClockPatternSpec extends AnyFlatSpec with Matchers {

  import ScalaSourceScan.{codeOf, read, scalaFiles}

  private val ClientRoot = "worker/src/main/scala/services/cinemas"
  private val Source     = s"$ClientRoot/common/ScraperParse.scala"

  private val ClockLiteral =
    """\\d\{(?:1,2|2)\}\)?(?::|\\\.|\[[.:]{2}\])\(?\\d\{2\}(?!\)?\\\.)""".r
  /** A two-argument `LocalTime.of(` / `.atTime(` whose first argument is not an integer literal. */
  private val NumbersClock = """(?:LocalTime\.of|\.atTime)\(\s*(?!\d)[^,()]*(?:\([^()]*\))?[^,()]*,""".r

  private[tools] def clockRules(src: String): Seq[Int] =
    Seq(ClockLiteral, NumbersClock)
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
    // The shapes the first cut let through.
    clockRules("""^(\d{1,2})\.(\d{2})$""") shouldBe Seq(1)
    clockRules("LocalTime.of(parts(0).toInt, parts(1).toInt)") shouldBe Seq(1)
    clockRules("Try(LocalTime.of(h.toInt, m.toInt)).toOption") shouldBe Seq(1)
    clockRules("Try(LocalTime.of(hour24, m.group(2).toInt)).toOption") shouldBe Seq(1)
    clockRules("Try(d.atTime(m.group(4).toInt, m.group(5).toInt)).toOption") shouldBe Seq(1)
    clockRules("date.atTime(hh, mm)") shouldBe Seq(1)
    // A day-first date, a constant time and a whole-LocalTime atTime are no clock read.
    clockRules("""(\d{2}\.\d{2}\.\d{4})""") shouldBe empty
    clockRules("""(\d{2})\.(\d{2})\.(\d{4})""") shouldBe empty
    clockRules("LocalTime.of(18, 0)") shouldBe empty
    clockRules("date.atTime(time)") shouldBe empty
  }

  "the cinema clients" should "spell and read a clock only through ScraperParse" in {
    val sources = scalaFiles(Seq(ClientRoot)).filterNot(_.toString == Source)
    sources.size should be > 100
    val found = sources.flatMap { path =>
      clockRules(codeOf(path)).map(line => s"$path:$line: ${read(Paths.get(path.toString)).linesIterator.drop(line - 1).next().trim}")
    }
    withClue("Compose the clock from ScraperParse's fragments (raw\"\"\"…${ScraperParse.ClockParts}…\"\"\") and read it with " +
      "ScraperParse.clock / clockAt / meridiemClockAt or parseHHmm, instead of a pattern of the client's own:\n" +
      found.mkString("\n") + "\n")(found shouldBe empty)
  }
}

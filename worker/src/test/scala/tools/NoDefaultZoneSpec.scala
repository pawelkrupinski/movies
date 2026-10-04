package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}

/**
 * Production code reads a local date or time ONLY through `models.VenueClock`, and names a zone
 * only through `models.TimeZones` (or a venue's `City`).
 *
 * The class of failure, found a dozen times over: a date read once at boot froze every scraper on
 * its boot day; US venues were given Warsaw's day, Los Angeles New York's, the Canaries Madrid's;
 * metrics judged "upcoming" in the pod's UTC through `Clock.systemDefaultZone()`. Each was a
 * `LocalDate.now(…)` or a zone-id string written where the code happened to need it. Rules, each
 * naming file:line:
 *
 *  1. No `X.now(…)` for any local/zoned java.time type — ask `VenueClock` (`today`, `now`,
 *     `todayAt`, `nowIn`), built from the injected Clock.
 *  2. No JVM default zone: `ZoneId.systemDefault`, `TimeZone.getDefault`/`getTimeZone`,
 *     `Clock.systemDefaultZone`, `Calendar.getInstance()`, `SimpleDateFormat` — and no `ZoneId.of`
 *     outside `TimeZones` (`TimeZones.named` for an id that roster DATA carries).
 *  3. No zone-id string literal ("Europe/Warsaw") outside `TimeZones` and the roster data files.
 *  4. No parameter that DEFAULTS to the live clock (`clock: Clock = Clock.systemUTC()`,
 *     `today: => LocalDate = VenueClock.system.todayInPoland`). A default lets the composition
 *     root forget to pass its clock and nothing notices: when this rule landed, twenty production
 *     components (the task worker, every census and metric, the chunk planner, the reapers) and
 *     two UK chains were silently on the system clock instead of the wiring's. The live clock is
 *     read once, at a composition root (`WorkerWiring.clock`, `Wiring.clock`, a tool's `main`).
 */
class NoDefaultZoneSpec extends AnyFlatSpec with Matchers {

  import ScalaSourceScan.{MainRoots, codeOf, parameterDefaults, read, scalaFiles}

  private val ZoneSource = "common/src/main/scala/models/TimeZones.scala"
  private val RosterData =
    "roster DATA: each venue or metro carries the zone it keeps, read through TimeZones.named"

  /** file → why it may name a zone id as a string. */
  private val ZoneLiteralAllowlist: Map[String, String] = Map(
    ZoneSource                                                       -> "the zone source itself",
    "common/src/main/scala/models/UsRosterData.scala"               -> RosterData,
    "common/src/main/scala/models/SpanishRosterData.scala"          -> RosterData,
    "worker/src/main/scala/services/cinemas/us/UsChainVenues.scala" -> RosterData)

  private val NoClockSeam =
    "TODO(clock-default-backlog): built where no clock is in scope (a trait's default member, the Mongo " +
      "repositories, which have no Clock seam yet); thread one through and drop the default"

  /** `file: Owner.parameter` → why that main-code parameter may still default to the live clock. */
  private val ClockDefaultAllowlist: Map[String, String] = Map(
    "common/src/main/scala/services/movies/ChangeStreamLiveness.scala: ChangeStreamLiveness.clock" -> NoClockSeam,
    "common/src/main/scala/services/movies/MovieChangeStream.scala: MovieChangeStream.clock"       -> NoClockSeam,
    "common/src/main/scala/services/movies/EmbeddedYear.scala: ofAll.maxYear" ->
      "a plausibility cap on a year read out of a title (next year at most); only New Year moves it, never a venue's day",
  )

  private val LiveClockDefault = """\bClock\.system(?:UTC|DefaultZone)\(|\bVenueClock\.system\b""".r

  private lazy val clockDefaults: Seq[String] =
    parameterDefaults(MainRoots).collect { case d if LiveClockDefault.findFirstIn(d.default).isDefined => d.label }

  private val LocalNow     = """\b(?:LocalDate|LocalDateTime|LocalTime|ZonedDateTime|OffsetDateTime|OffsetTime|YearMonth|Year|MonthDay)\.now\(""".r
  private val DefaultZone  =
    """\bZoneId\.systemDefault\b|\bTimeZone\.(?:getDefault|getTimeZone)\b|\bClock\.systemDefaultZone\b|\bCalendar\.getInstance\(\s*\)|\bnew\s+(?:java\.text\.)?SimpleDateFormat\(""".r
  private val ZoneOf       = """\bZoneId\.of\(""".r
  private val ZoneLiteral  = """"(?:Africa|America|Antarctica|Asia|Atlantic|Australia|Europe|Indian|Pacific)/[A-Za-z_]+""".r

  private def linesMatching(src: String, patterns: Seq[scala.util.matching.Regex]): Seq[Int] =
    patterns.flatMap(_.findAllMatchIn(src).map(m => src.substring(0, m.start).count(_ == '\n') + 1)).distinct.sorted

  /** Rules 1–2 for `path` (comment-free source). */
  private[tools] def zoneReads(path: String, src: String): Seq[Int] =
    linesMatching(src, Seq(LocalNow, DefaultZone) ++ Option.when(path != ZoneSource)(ZoneOf))

  private[tools] def zoneLiterals(src: String): Seq[Int] = linesMatching(src, Seq(ZoneLiteral))

  private lazy val sources = scalaFiles(MainRoots).map(p => p.toString -> codeOf(p))

  private def report(found: Seq[(String, Int)]): Seq[String] = found.map { case (path, line) =>
    s"$path:$line: ${read(Paths.get(path)).linesIterator.drop(line - 1).next().trim}"
  }

  "the zone matchers" should "catch each way of reading the default zone or a zone of one's own" in {
    zoneReads("x", """val d = LocalDate.now(ZoneId.of("Europe/Warsaw"))""") shouldBe Seq(1)
    zoneReads("x", "LocalDateTime.now(clock.withZone(city.zoneId))") shouldBe Seq(1)
    zoneReads("x", "java.time.Year.now().getValue") shouldBe Seq(1)
    zoneReads("x", "clock: Clock = Clock.systemDefaultZone()") shouldBe Seq(1)
    zoneReads("x", "instant.atZone(ZoneId.systemDefault)") shouldBe Seq(1)
    zoneReads("x", "java.util.TimeZone.getDefault") shouldBe Seq(1)
    zoneReads("x", "ZoneId.of(venue.zoneId)") shouldBe Seq(1)
    zoneReads(ZoneSource, "def named(id: String): ZoneId = ZoneId.of(id)") shouldBe empty
    zoneReads("x", "venueClock.today(TimeZones.Poland)") shouldBe empty
    zoneReads("x", "LocalDate.ofInstant(clock.instant(), zone)") shouldBe empty
    zoneReads("x", "Instant.now()") shouldBe empty                 // an instant has no zone (NoWallClockInTestsSpec)
    zoneLiterals(""""Europe/Warsaw"""") shouldBe Seq(1)
    zoneLiterals(""""America/Los_Angeles"""") shouldBe Seq(1)
    zoneLiterals(""""Europe"""") shouldBe empty
  }

  "production code" should "read local dates and times only through VenueClock, and never the JVM's zone" in {
    sources.size should be > 100
    val found = report(sources.flatMap { case (path, src) => zoneReads(path, src).map(path -> _) })
    withClue("Ask models.VenueClock (built from the injected Clock) for the day or hour, in a zone named by " +
      "models.TimeZones or the venue's City:\n" + found.mkString("\n") + "\n")(found shouldBe empty)
  }

  it should "name zone ids only in TimeZones and the roster data" in {
    val found = report(sources.filterNot { case (path, _) => ZoneLiteralAllowlist.contains(path) }
      .flatMap { case (path, src) => zoneLiterals(src).map(path -> _) })
    withClue("Use a models.TimeZones constant (add one there), or allowlist a data file with a reason:\n" +
      found.mkString("\n") + "\n")(found shouldBe empty)
  }

  it should "not default a parameter to the live clock — the composition root passes its own" in {
    val found = clockDefaults.filterNot(ClockDefaultAllowlist.contains)
    withClue("These parameters default to the live clock, so a caller holding the wiring's clock can forget it. " +
      "Drop the default and pass the clock (or a VenueClock built from it) from the composition root:\n" +
      found.mkString("\n") + "\n")(found shouldBe empty)
  }

  it should "keep every clock-default allowlist entry still defaulting (the backlog only shrinks)" in {
    (ClockDefaultAllowlist.keySet -- clockDefaults.toSet).toSeq.sorted shouldBe empty
  }

  it should "keep every zone-literal allowlist entry pointing at a file that still names one" in {
    val stale = ZoneLiteralAllowlist.keys.toSeq.sorted.filterNot { file =>
      Files.exists(Paths.get(file)) && zoneLiterals(codeOf(Paths.get(file))).nonEmpty
    }
    withClue("Allowlisted but naming no zone any more — drop the entry:\n" + stale.mkString("\n") + "\n")(stale shouldBe empty)
  }
}

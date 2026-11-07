package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import java.time.{LocalDate, ZoneOffset}

/**
 * Specs must not depend on what day it is.
 *
 * The class of failure: a spec passes today and fails on some later date nobody picked.
 * Four depth-guard specs scraped showtimes fixed at 2027-06-08 into a `CaffeineMovieCache`
 * built without a clock, so the cache measured "upcoming" against the SYSTEM clock — every
 * rejection they asserted would have turned into an accept the day those showtimes went
 * by (fixed in ea1fa3c78, found only by moving the dates into the past by hand).
 *
 * Two rules, each naming file:line:
 *
 *  1. No wall-clock READ in test sources — `Instant.now()`, `LocalDate(Time).now()`,
 *     `System.currentTimeMillis()`, `Clock.systemUTC()`, `new Date()` — outside the allowlist
 *     below, each entry with its reason. A default parameter is no exception: a defaulted clock
 *     is one a caller forgets to pin. Take the time from a fixed `Clock` and hand the same
 *     clock to the code under test; measure elapsed time with `System.nanoTime()`, which is
 *     not a date.
 *
 *  2. A test file holding a date literal still in the FUTURE must hand a clock to every
 *     production class whose constructor otherwise defaults it to the system clock. That
 *     is the depth-guard shape exactly: the date is fixed, the clock is not, and the day
 *     they cross is the day the spec flips. A file is also held to it when it uses a
 *     helper declared in a file with such a literal (`DepthGuardTime.showtimes(...)`).
 *     This rule reads today's date itself, deliberately: a literal already in the past can
 *     only stay past, so the verdict can go from failing to passing over time, never back.
 *
 * Rule 5 holds production code to the same: main sources read the time — wall or monotonic — only at a
 * composition root (`Wiring`, `WorkerWiring`, `WorkerMain`, a tool's `main`), which hands its `clock` and
 * `stopwatch` down; every other class takes them as parameters.
 *
 * Runnable programs under the test trees (`def main` — backfills, recorders, probes) talk to the
 * live world, so rules 1–2 do not fit them; rule 4 holds them instead: a program reads the wall
 * clock through ONE `Clock.systemUTC()`, built where it is wired and handed to everything that
 * needs the time, exactly as the worker's composition root does. Without it a tool's "today" is
 * read in five places, and the logic under it cannot be run at a pinned date by a spec.
 */
class NoWallClockInTestsSpec extends AnyFlatSpec with Matchers {

  // Why a file may still read the wall clock. Each reason names the production code that reads
  // it without a Clock seam — the spec's data has to be relative to that same clock — so the
  // way to shrink this list is to give that code a Clock, not to widen the list.
  private val MongoPath =
    "written while the Mongo repositories stamped updatedAt with the system clock: it still hands them " +
      "Clock.systemUTC() and dates its showtimes off the wall clock — to move onto a pinned clock, which the " +
      "repositories now take (2026-10-20)"
  private val FileAges =
    "the fixture enrichment cache ages entries by file mtime against the real clock (FileTime); it records live " +
      "responses, outside any spec's timeline"

  /** file → why it may read the wall clock. Keep each reason specific enough to check. */
  private val Allowlist: Map[String, String] = Map(
    "worker/src/it/scala/LiveFamilySources.scala" ->
      "a live capture/fill tool against Filmweb: asks for today's programmes, which only the real date names (the stores keep their pinned clock)",
    "worker/src/test/scala/tools/NoWallClockInTestsSpec.scala" ->
      "rule 2 compares literals with today on purpose (see the class doc)",
    "worker/src/test/scala/tools/NoDefaultZoneSpec.scala" ->
      "its self-test feeds the zone matcher these reads as string literals",
    "http/src/test/scala/tools/TlsTrustSpec.scala" ->
      "documents that a pinned certificate really is expired — a statement about today by design",
    "worker/src/fixtures/scala/tools/ArchiveReplayWiring.scala" ->
      "a RECORDING's requests go out live, so its pacers measure their slots in real time (a hermetic replay keeps the frozen clock)",
    "worker/src/fixtures/scala/tools/FileEnrichmentCacheStore.scala"    -> FileAges,
    "worker/src/fixtures/scala/tools/EnrichmentFreshness.scala"         -> FileAges,
    "worker/src/test/scala/tools/FileEnrichmentCacheStoreSpec.scala"    -> FileAges,
    "worker/src/test/scala/tools/EnrichmentFreshnessSpec.scala"         -> FileAges
  ) ++ Seq(
    "MongoTaskQueueIntegrationSpec",
    "MovieRepositoryIntegrationSpec",
    "MoviesWriteSkippedWhenUnchangedIntegrationSpec", "ProdCoverageIntegrationSpec", "StrandedSideRowsIntegrationSpec",
    "VenueRosterIntegrationSpec",
    "RepositoryWriteFailureIntegrationSpec", "RetiredVenueRowsIntegrationSpec", "ScanStitchedPagingSpec",
    "ScreeningsRewriteOnUpsertIntegrationSpec", "SideRowIdScanPagingSpec", "SlotsWatchProjectionIntegrationSpec",
    "UnreadyRoundTripProjectionIntegrationSpec"
  ).map(spec => s"worker/src/it/scala/$spec.scala" -> MongoPath)

  import ScalaSourceScan.{TestRoots, argumentsAt, code, isProgram, read, scalaFiles}

  /** `X.now(…)` or the paren-less `X.now` (`LocalDateTime.now.plusDays(1)` reads the clock just the same). */
  private val Now = """\b(?:Instant|LocalDate|LocalDateTime|LocalTime|ZonedDateTime|OffsetDateTime|Year|YearMonth)\.now\b""".r

  /** Whether the `now` matched at `m` reads the wall clock: always paren-less, and with arguments unless handed a clock. */
  private def readsWallClock(line: String, m: scala.util.matching.Regex.Match): Boolean = {
    val open = m.end + line.substring(m.end).takeWhile(_ == ' ').length
    open >= line.length || line.charAt(open) != '(' ||
      !argumentsAt(line, open).toLowerCase(java.util.Locale.ROOT).contains("clock")
  }
  private val OtherReads = """System\.currentTimeMillis\(|Clock\.system(?:UTC|DefaultZone)\(|new (?:java\.util\.)?Date\(\)""".r

  private[tools] def wallClockReads(line: String): Seq[Int] = {
    val nowCalls = Now.findAllMatchIn(line).collect { case m if readsWallClock(line, m) => m.start }
    (nowCalls ++ OtherReads.findAllMatchIn(line).map(_.start)).toSeq
  }

  private val testFiles: Seq[Path] = scalaFiles(TestRoots).filterNot(isProgram)

  "the test-source matcher" should "refuse a wall-clock read even as a parameter's default" in {
    wallClockReads("def f(now: () => Long = () => System.currentTimeMillis()) = now()") should have size 1
    wallClockReads("def f(at: Instant = Instant.now()) = at") should have size 1
    wallClockReads("class W(wallClock: Clock = Clock.systemUTC())") should have size 1
    wallClockReads("val today = LocalDate.now(clock)") shouldBe empty
    wallClockReads("Seq(Showtime(LocalDateTime.now.plusDays(1), bookingUrl = None))") should have size 1
    wallClockReads("val t = Instant.now") should have size 1
  }

  "Test sources" should "not read the wall clock outside the allowlist" in {
    testFiles.nonEmpty shouldBe true
    val offenders = testFiles.filterNot(p => Allowlist.contains(p.toString)).flatMap { path =>
      read(path).linesIterator.zipWithIndex.collect {
        case (line, index) if wallClockReads(code(line)).nonEmpty => s"$path:${index + 1}: ${line.trim}"
      }
    }
    withClue(
      "These test lines read the wall clock. Take the time from a fixed Clock and pass the same clock to the " +
        "code under test (System.nanoTime() for elapsed time), or allowlist the file with a reason:\n" +
        offenders.mkString("\n") + "\n") {
      offenders shouldBe empty
    }
  }

  it should "keep every allowlist entry pointing at a file that still reads the clock" in {
    val stale = Allowlist.keys.toSeq.sorted.filterNot { file =>
      val path = Paths.get(file)
      Files.exists(path) && read(path).linesIterator.exists(line => wallClockReads(code(line)).nonEmpty)
    }
    withClue("Allowlisted but no longer reading the wall clock — drop the entry:\n" + stale.mkString("\n") + "\n") {
      stale shouldBe empty
    }
  }

  // ── rule 4 ─────────────────────────────────────────────────────────────────

  /** A program's wall-clock source — `Clock.systemUTC()`, or `VenueClock.system`, which wraps one. */
  private val SystemClock = """Clock\.systemUTC\(|\bVenueClock\.system\b""".r
  /** A system clock in a zone: a second wall-clock source beside the program's one. */
  private val ZonedSystemClock = """Clock\.system\(""".r
  private val VenueSystemClock = """\bVenueClock\.system\b""".r

  /** Each wall-clock read on the line: a `now(…)` not handed a clock, or one of the other reads. */
  private def programReads(line: String): Seq[String] =
    Now.findAllMatchIn(line).collect { case m if readsWallClock(line, m) => m.matched }.toSeq ++
      (OtherReads.findAllMatchIn(line) ++ ZonedSystemClock.findAllMatchIn(line) ++ VenueSystemClock.findAllMatchIn(line)).map(_.matched)

  /** Every wall-clock read in a program's source but its one `Clock.systemUTC()`, as `file:line: text`. */
  private[tools] def programOffenders(path: String, src: String): Seq[String] = {
    val reads = src.linesIterator.zipWithIndex.flatMap { case (raw, index) =>
      programReads(code(raw)).map(read => (read, s"$path:${index + 1}: ${raw.trim}"))
    }.toSeq
    val (systemClocks, others) = reads.partition { case (read, _) => SystemClock.findFirstIn(read).isDefined }
    (others ++ (if (systemClocks.size > 1) systemClocks else Nil)).map(_._2)
  }

  "the program matcher" should "keep one Clock.systemUTC() and refuse every other read" in {
    programOffenders("A.scala", "val clock = Clock.systemUTC()\nval today = LocalDate.now(clock)") shouldBe empty
    programOffenders("A.scala", "val clock = Clock.systemUTC()\nval t = Clock.systemUTC()") should have size 2
    programOffenders("A.scala", "LocalDate.now(ZoneId.of(\"UTC\"))") shouldBe Seq("A.scala:1: LocalDate.now(ZoneId.of(\"UTC\"))")
    programOffenders("A.scala", "val t0 = System.currentTimeMillis()\nval n = Instant.now()") should have size 2
    programOffenders("A.scala", "def f(at: Instant = Instant.now()) = at") should have size 1
    programOffenders("A.scala", "val clock = Clock.systemUTC()\nval zoned = Clock.system(zone)") should have size 1
    programOffenders("A.scala", "val clock = Clock.systemUTC()\nval venues = VenueClock.system") should have size 2
    programOffenders("A.scala", "val venues = VenueClock.system\nval today = venues.todayInPoland") shouldBe empty
  }

  "Programs under the test trees" should "read the wall clock only through one Clock.systemUTC(), handed down" in {
    val programs = scalaFiles(TestRoots).filter(isProgram)
    programs.nonEmpty shouldBe true
    val offenders = programs.flatMap(path => programOffenders(path.toString, read(path)))
    withClue(
      "These programs read the wall clock outside their one Clock.systemUTC(). Build `val clock = Clock.systemUTC()` " +
        "once where the program is wired, pass it down (LocalDate.now(clock), clock.instant()), and time work with " +
        "tools.Stopwatch:\n" + offenders.mkString("\n") + "\n") {
      offenders shouldBe empty
    }
  }

  // ── rule 3 ─────────────────────────────────────────────────────────────────
  //
  // A production parameter that DEFAULTS an instant to the wall clock (`at: Instant =
  // Instant.now()`) lets a caller who holds the injected clock forget to pass it, and nothing
  // notices until two components judge one stamp on different clocks — the detail reaper and
  // handler disagreed about "fresh" exactly that way, and a harness pinned to 2026-06-08 then
  // re-asked for work a system-clock stamp said was done. A `() => Instant` function default
  // (`now: () => Instant = () => Instant.now()`) is refused alike — it escapes the wiring's clock
  // just as silently; a `clock: Clock = Clock.systemUTC()` default is refused by NoDefaultZoneSpec
  // rule 4. An elapsed-time seam defaulted to `System.nanoTime` is refused too: durations come from the
  // wiring's `tools.Stopwatch`, handed down like its clock.

  /** `file:parameter` → why that main-source parameter may still default to the wall clock. */
  private val MainDefaultAllowlist: Map[String, String] = Map.empty

  /** A time read: the wall clock's (`X.now()` / paren-less `X.now`, `currentTimeMillis`) or the monotonic one's
   *  (`System.nanoTime`, `Stopwatch.System`'s source) — a defaulted elapsed-time seam escapes the wiring's
   *  stopwatch exactly as a defaulted clock escapes its clock. */
  private val MainWallClockRead =
    """\b(?:Instant|LocalDate|LocalDateTime|ZonedDateTime|OffsetDateTime)\.now(?:\(\s*\)|\b(?!\s*\())|System\.(?:currentTimeMillis|nanoTime)\(""".r
  /** `name: Type = ` right before the read — a value default, or a function one (`= () =>`). */
  private val ParameterName     = """(?:^\s*|[(,]\s*)(\w+)\s*:(?:=>|\(\)\s*=>|[^=,(])+=(?!>)\s*(?:\(\)\s*=>\s*)?$""".r

  /** `parameter` → `line: text` for each parameter in `src` defaulting to a wall-clock read. */
  private[tools] def clockDefaultsIn(src: String): Seq[(String, String)] =
    src.linesIterator.zipWithIndex.flatMap { case (raw, index) =>
      val line = code(raw)
      MainWallClockRead.findAllMatchIn(line).flatMap { m =>
        val prefix = line.substring(0, m.start).replaceAll("""(?:java\.(?:time|lang)\.)$""", "")
        ParameterName.findFirstMatchIn(prefix).map(p => p.group(1) -> s"${index + 1}: ${raw.trim}")
      }
    }.toSeq

  private def mainClockDefaults: Seq[(String, String)] =
    scalaFiles(ScalaSourceScan.MainRoots).flatMap { path =>
      clockDefaultsIn(read(path)).map { case (parameter, at) => s"$path:$parameter" -> s"$path:$at" }
    }

  "the main-source default matcher" should "refuse value and function defaults alike, on the wall clock or the monotonic one" in {
    clockDefaultsIn("def f(at: Instant = Instant.now()) = at").map(_._1) shouldBe Seq("at")
    clockDefaultsIn("class R(clock: () => java.time.Instant = () => java.time.Instant.now(),").map(_._1) shouldBe Seq("clock")
    clockDefaultsIn("  now: () => Long = () => System.currentTimeMillis()").map(_._1) shouldBe Seq("now")
    clockDefaultsIn("  nanoTime: () => Long = () => System.nanoTime(),").map(_._1) shouldBe Seq("nanoTime")
    clockDefaultsIn("  now: () => Long = () => System.nanoTime() / 1_000_000L,").map(_._1) shouldBe Seq("now")
    clockDefaultsIn("def f(at: Instant = Instant.now) = at").map(_._1) shouldBe Seq("at")
    clockDefaultsIn("val at = Instant.now()") shouldBe empty
  }

  "Main sources" should "not default a parameter to a wall-clock READ — take the injected clock's instant instead" in {
    val offenders = mainClockDefaults.filterNot { case (key, _) => MainDefaultAllowlist.contains(key) }.map(_._2)
    withClue("These parameters default to a time read, so a caller holding the wiring's clock or stopwatch can silently " +
      "skip it. Drop the default and pass the wiring's `clock` / `stopwatch`, or allowlist the parameter with a reason:\n" +
      offenders.mkString("\n") + "\n") {
      offenders shouldBe empty
    }
  }

  it should "keep every allowlisted parameter still defaulting to the wall clock" in {
    val live = mainClockDefaults.map(_._1).toSet
    (MainDefaultAllowlist.keySet -- live) shouldBe empty
  }

  // ── rule 5 ─────────────────────────────────────────────────────────────────
  //
  // A class that reads the process's time itself — `Instant.now()` stamping a Mongo row, a throttle on
  // `System.currentTimeMillis()`, `Stopwatch.System` reached for in place of a passed stopwatch — is one a spec
  // cannot pin and a replay cannot freeze: the screenings and slots stores stamped `updatedAt` that way until
  // 2026-10-20, so every it/ spec over them had to build its rows relative to the real clock. Production reads
  // the time only where it is wired, and passes it down.

  private val CompositionRoot = "a composition root: reads the process's clock and stopwatch once and hands them down"
  private val WaitPrimitive   =
    "a blocking wait's deadline (Object.wait / Thread.sleep): it must be read on the real time the wait blocks on, so a " +
      "passed stopwatch a spec steps would only desynchronise the two"

  /** Main-source file → why it may read the process's time directly. */
  private val MainTimeSources: Map[String, String] = Map(
    "web/src/main/scala/modules/Wiring.scala"            -> CompositionRoot,
    "worker/src/main/scala/modules/WorkerWiring.scala"   -> CompositionRoot,
    "worker/src/main/scala/modules/WorkerMain.scala"     -> CompositionRoot,
    "base/src/main/scala/tools/Stopwatch.scala"          -> "defines Stopwatch.System, the process's monotonic source",
    "common/src/main/scala/models/VenueClock.scala"      -> "defines VenueClock.system, a launcher's venue clock over Clock.systemUTC()",
    "worker/src/main/scala/tools/ThreadCpuClock.scala"   -> "a time source itself: thread CPU time, the monotonic clock where that is unsupported",
    "scrapers/src/main/scala/services/cinemas/CinemaEgress.scala" ->
      "CinemaEgress.diagnostic: the catalog's diagnostic door, which the identity tools compile into older refs, so it takes no stopwatch",
    "scrapers/src/main/scala/tools/ParallelDetailFetch.scala" ->
      ("FetchClock.System: a fetch timeout's deadline, read on the time its blocking get waits on (a spec swaps both through " +
        "`timed`), and reached from every client of that same diagnostic door"),
    "worker/src/main/scala/services/tasks/TaskWorker.scala"         -> WaitPrimitive,
    "worker/src/main/scala/services/readmodel/ReadModelSweeper.scala" -> WaitPrimitive
  )

  /** A read of the process's time in production code: the wall clock's, or the monotonic one's. */
  private val ProcessTime = """System\.nanoTime\(|\bStopwatch\.System\b|\bVenueClock\.system\b|\bClock\.system\(""".r

  private[tools] def processTimeReads(line: String): Seq[Int] =
    (wallClockReads(line) ++ ProcessTime.findAllMatchIn(line).map(_.start)).distinct

  /** The companion's `Stopwatch.start()` / `timed` / `total()`: [[Stopwatch.System]] by another name. */
  private val SystemStopwatchHelper = """\bStopwatch\.(?:start|timed|total)\b""".r

  /** Main-source files still timing work on the companion's helpers rather than a passed stopwatch — the backlog
   *  this rule froze on 2026-10-20 (it only shrinks). The identity resolver's (AgreementStage, IncrementalResolver,
   *  ObservationReads) change the resolver closure, which re-resolves every worker on deploy (batch them); the scrape
   *  decorators sit behind the catalog's diagnostic door. */
  private val SystemStopwatchBacklog: Set[String] = Set(
    "http/src/main/scala/tools/RetryWithBackoff.scala",
    "common/src/main/scala/tools/MonitoringHttpFetch.scala",
    "identity/src/main/scala/services/identity/ObservationReads.scala",
    "identity/src/main/scala/services/identity/IncrementalResolver.scala",
    "identity/src/main/scala/services/identity/agreement/AgreementStage.scala",
    "scrapers/src/main/scala/services/cinemas/common/CinemaScrapeRunner.scala",
    "scrapers/src/main/scala/services/cinemas/common/AdaptiveTimeoutScraper.scala",
    "scrapers/src/main/scala/services/cinemas/common/UptimeRecordingScraper.scala",
    "scrapers/src/main/scala/services/cinemas/common/SourceFallbackScraper.scala",
    "worker/src/main/scala/tools/AdaptiveParallel.scala",
    "worker/src/main/scala/services/tasks/ScrapeCinemaHandler.scala",
    "worker/src/main/scala/services/tasks/TaskWorker.scala",
    "worker/src/main/scala/services/identity/adapters/IdentityProjection.scala",
    "worker/src/main/scala/services/identity/adapters/IdentityModelService.scala",
    "worker/src/main/scala/services/readmodel/ReadModelProjector.scala",
    "worker/src/main/scala/services/movies/CorpusMirror.scala",
    "worker/src/main/scala/services/enrichment/CacheRefresher.scala",
    "worker/src/main/scala/services/enrichment/ImdbRatings.scala"
  )

  private lazy val mainFiles: Seq[Path] = scalaFiles(ScalaSourceScan.MainRoots)

  private def offendingLines(path: Path, reads: String => Seq[Int]): Seq[String] =
    read(path).linesIterator.zipWithIndex.collect {
      case (line, index) if reads(code(line)).nonEmpty => s"$path:${index + 1}: ${line.trim}"
    }.toSeq

  "the production time matcher" should "see every way of reading the process's time, and not a passed clock's" in {
    Seq("val at = Instant.now()", "val t0 = System.nanoTime()", "if (System.currentTimeMillis() - last > 5000L)",
      "new LogThrottle(interval, Stopwatch.System)", "val today = VenueClock.system.todayInPoland",
      "val zoned = Clock.system(zone)", "val clock = Clock.systemUTC()", "val at = Instant.now")
      .foreach(line => withClue(line)(processTimeReads(line) should have size 1))
    Seq("val at = clock.instant()", "val started = stopwatch.start()", "LocalDate.now(clock)", "venueClock.todayInPoland")
      .foreach(line => withClue(line)(processTimeReads(line) shouldBe empty))
    SystemStopwatchHelper.findFirstIn("val handled = tools.Stopwatch.timed(Try(h.handle(task)))") shouldBe defined
    SystemStopwatchHelper.findFirstIn("val handled = stopwatch.timed(Try(h.handle(task)))") shouldBe None
  }

  "Main sources" should "read the process's time only at a composition root, which passes its clock and stopwatch down" in {
    val offenders = mainFiles.filterNot(p => MainTimeSources.contains(p.toString)).flatMap { path =>
      if (ScalaSourceScan.definesMain(read(path))) programOffenders(path.toString, read(path))
      else offendingLines(path, processTimeReads)
    }
    withClue("These production lines read the process's time themselves. Take a `clock: java.time.Clock` or a " +
      "`stopwatch: tools.Stopwatch` parameter, passed from the composition root (Wiring / WorkerWiring), or name the " +
      "file in MainTimeSources with why it cannot:\n" + offenders.mkString("\n") + "\n") {
      offenders shouldBe empty
    }
  }

  it should "time work on a passed stopwatch, not the companion's helpers (the backlog only shrinks)" in {
    val users = mainFiles.filter(p => offendingLines(p, line => SystemStopwatchHelper.findAllMatchIn(line).map(_.start).toSeq).nonEmpty)
      .map(_.toString).filterNot(_.endsWith("tools/Stopwatch.scala")).toSet
    withClue("New users of Stopwatch.start()/timed/total() — take the wiring's `stopwatch` and call it instead:\n")(
      (users -- SystemStopwatchBacklog).toSeq.sorted shouldBe empty)
    withClue("No longer using the companion's helpers — drop from SystemStopwatchBacklog:\n")(
      (SystemStopwatchBacklog -- users).toSeq.sorted shouldBe empty)
  }

  it should "keep every MainTimeSources entry pointing at a file that still reads the time" in {
    val stale = MainTimeSources.keys.toSeq.sorted.filterNot { file =>
      val path = Paths.get(file)
      Files.exists(path) && offendingLines(path, processTimeReads).nonEmpty
    }
    withClue("Listed in MainTimeSources but no longer reading the time — drop the entry:\n" + stale.mkString("\n") + "\n")(stale shouldBe empty)
  }

  // ── rule 2 ─────────────────────────────────────────────────────────────────

  private val ClockDefault = """clock\s*:\s*(?:java\.time\.)?Clock\s*=\s*(?:java\.time\.)?Clock\.system""".r
  private val ClassHeader  = """\bclass\s+(\w+)\s*(?:\[[^\]]*\])?\s*(?=\()""".r

  /** Production classes whose constructor defaults `clock` to the system clock. */
  private lazy val clockDefaulted: Set[String] =
    scalaFiles(ScalaSourceScan.MainRoots).flatMap(path => clockDefaultedIn(read(path).linesIterator.map(code).mkString("\n"))).toSet

  private def clockDefaultedIn(src: String): Seq[String] =
    ClassHeader.findAllMatchIn(src).collect {
      case m if ClockDefault.findFirstIn(argumentsAt(src, m.end)).isDefined => m.group(1)
    }.toSeq

  private val DateLiteral   = """"(20\d\d)-(\d\d)(?:-(\d\d))?""".r
  private val DateOfLiteral = """Local(?:Date|DateTime)\.of\(\s*(20\d\d)\s*,\s*(\d{1,2})\s*,\s*(\d{1,2})""".r

  private def futureDates(src: String, today: LocalDate): Seq[LocalDate] =
    (DateLiteral.findAllMatchIn(src) ++ DateOfLiteral.findAllMatchIn(src)).flatMap { m =>
      scala.util.Try(LocalDate.of(m.group(1).toInt, m.group(2).toInt, Option(m.group(3)).fold(1)(_.toInt))).toOption
    }.filter(_.isAfter(today)).toSeq

  /** A top-level helper (not a spec) a test file declares — what another file can call. */
  private val Declaration = """(?m)^(?:(?:final|private|case|sealed|abstract)\s+)*(?:object|class|trait)\s+(\w+)""".r

  "A test file with a future date literal" should "hand a clock to every class that would otherwise read the system clock" in {
    val today   = LocalDate.now(ZoneOffset.UTC)
    val sources = testFiles.map(p => p -> read(p).linesIterator.map(code).mkString("\n")).toMap
    val dated   = sources.collect { case (p, src) if futureDates(src, today).nonEmpty => p }.toSet
    // Helpers declared beside a future literal carry it into every file that uses them.
    val carriers = dated.toSeq.flatMap(p => Declaration.findAllMatchIn(sources(p)).map(_.group(1)).filterNot(_.endsWith("Spec")).map(_ -> p)).toMap
    val held: Map[Path, String] = sources.keys.flatMap { p =>
      if (dated(p)) Some(p -> s"dates it after $today")
      else carriers.collectFirst {
        case (name, from) if from != p && s"""\\b$name\\.""".r.findFirstIn(sources(p)).isDefined =>
          p -> s"uses $name, which dates it after $today in $from"
      }
    }.toMap

    // The scan sees a defaulted clock (NoDefaultZoneSpec refuses one in main code, so none is left to find there).
    clockDefaultedIn("final class Liveness(clock: Clock = Clock.systemUTC(), n: Int)\nclass Pinned(clock: Clock)") shouldBe Seq("Liveness")
    val constructions = clockDefaulted.toSeq.sorted.map(name => name -> s"""\\bnew\\s+$name\\b\\s*(?:\\[[^\\]]*\\])?\\s*\\(""".r)
    val offenders = held.toSeq.sortBy(_._1.toString).flatMap { case (path, why) =>
      val src = sources(path)
      constructions.flatMap { case (name, ctor) =>
        ctor.findAllMatchIn(src).collect {
          case m if !argumentsAt(src, m.end - 1).toLowerCase(java.util.Locale.ROOT).contains("clock") =>
            s"$path:${src.substring(0, m.start).count(_ == '\n') + 1}: new $name(…) without a clock — the file $why"
        }
      }
    }
    withClue(
      s"These files fix dates later than $today but build a class that measures them against the SYSTEM clock — " +
        "the spec flips the day the two cross. Pass a fixed clock set before the dates:\n" +
        offenders.mkString("\n") + "\n") {
      offenders shouldBe empty
    }
  }
}

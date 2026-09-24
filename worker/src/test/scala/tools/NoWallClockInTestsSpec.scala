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
 *     `System.currentTimeMillis()`, `Clock.systemUTC()`, `new Date()` — outside a default
 *     parameter (a seam the caller can pin) or the allowlist below, each entry with its
 *     reason. Take the time from a fixed `Clock` and hand the same clock to the code under
 *     test; measure elapsed time with `System.nanoTime()`, which is not a date.
 *
 *  2. A test file holding a date literal still in the FUTURE must hand a clock to every
 *     production class whose constructor otherwise defaults it to the system clock. That
 *     is the depth-guard shape exactly: the date is fixed, the clock is not, and the day
 *     they cross is the day the spec flips. A file is also held to it when it uses a
 *     helper declared in a file with such a literal (`DepthGuardTime.showtimes(...)`).
 *     This rule reads today's date itself, deliberately: a literal already in the past can
 *     only stay past, so the verdict can go from failing to passing over time, never back.
 *
 * Runnable programs under the test trees (`def main` — backfills, recorders, probes) are
 * operator tools talking to the live world, not specs, and are out of scope.
 */
class NoWallClockInTestsSpec extends AnyFlatSpec with Matchers {

  private val TestRoots = Seq(
    "common/src/test", "testkit/src", "web/src/test", "web/src/it", "web/src/page",
    "worker/src/test", "worker/src/it", "worker/src/fixtures", "e2e/src/test")

  // Why a file may still read the wall clock. Each reason names the production code that reads
  // it without a Clock seam — the spec's data has to be relative to that same clock — so the
  // way to shrink this list is to give that code a Clock, not to widen the list.
  private val MongoPath =
    "the Mongo repositories stamp updatedAt and filter upcoming showtimes with the system clock (no Clock seam " +
      "on the Mongo path), so rows are built relative to it"
  private val FileAges =
    "the fixture enrichment cache ages entries by file mtime against the real clock (FileTime); it records live " +
      "responses, outside any spec's timeline"

  /** file → why it may read the wall clock. Keep each reason specific enough to check. */
  private val Allowlist: Map[String, String] = Map(
    "worker/src/test/scala/tools/NoWallClockInTestsSpec.scala" ->
      "rule 2 compares literals with today on purpose (see the class doc)",
    "common/src/test/scala/tools/TlsTrustSpec.scala" ->
      "documents that a pinned certificate really is expired — a statement about today by design",
    "worker/src/fixtures/scala/tools/FileEnrichmentCacheStore.scala"    -> FileAges,
    "worker/src/test/scala/tools/FileEnrichmentCacheStoreSpec.scala"    -> FileAges,
    "worker/src/test/scala/tools/EnrichmentFreshnessSpec.scala"         -> FileAges
  ) ++ Seq(
    "FoldFixture", "FreshnessStoreIntegrationSpec", "MergeScreeningsIntegrationSpec", "MongoTaskQueueIntegrationSpec",
    "MoveFilmDurabilitySpec", "MovieRepositoryIntegrationSpec", "MovieRepositoryUpdatedSinceIntegrationSpec",
    "MoviesWriteSkippedWhenUnchangedIntegrationSpec", "ProdCoverageIntegrationSpec", "RekeyScreeningsIntegrationSpec",
    "RepositoryWriteFailureIntegrationSpec", "RetiredVenueRowsIntegrationSpec", "RetryResolveServingIntegrationSpec", "ScanStitchedPagingSpec",
    "ScreeningsRewriteOnUpsertIntegrationSpec", "SideRowIdScanPagingSpec", "SlotsWatchProjectionIntegrationSpec",
    "StagingFoldIntegrationSpec", "UnreadyRoundTripProjectionIntegrationSpec"
  ).map(spec => s"worker/src/it/scala/$spec.scala" -> MongoPath) ++ Map(
    // Claims from the real Mongo queue, whose enqueue stamps `submittedAt` with the system clock.
    "worker/src/it/scala/contracts/ResolveDispatcherContractSpec.scala" -> MongoPath
  )

  import ScalaSourceScan.{argumentsAt, code, read, scalaFiles}

  private val Now = """\b(?:Instant|LocalDate|LocalDateTime|LocalTime|ZonedDateTime|OffsetDateTime|Year|YearMonth)\.now\(""".r
  private val OtherReads = """System\.currentTimeMillis\(|Clock\.system(?:UTC|DefaultZone)\(|new (?:java\.util\.)?Date\(\)""".r

  /** `name: Type = ` (or `= () =>`) right before the read: a parameter's default. */
  private val DefaultParameter = """(?:^\s*|[(,]\s*)\w+\s*:(?:=>|[^=])+=(?!>)\s*(?:\(\)\s*=>\s*)?$""".r

  private def wallClockReads(line: String): Seq[Int] = {
    val nowCalls = Now.findAllMatchIn(line).collect {
      case m if !argumentsAt(line, m.end - 1).toLowerCase.contains("clock") => m.start
    }
    (nowCalls ++ OtherReads.findAllMatchIn(line).map(_.start)).toSeq.filterNot { at =>
      val prefix = line.substring(0, at).replaceAll("""(?:java\.(?:time|util|lang)\.)$""", "")
      DefaultParameter.findFirstIn(prefix).isDefined
    }
  }

  private val testFiles: Seq[Path] = scalaFiles(TestRoots).filterNot(p => read(p).contains("def main("))

  "Test sources" should "not read the wall clock outside a default parameter or the allowlist" in {
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

  // ── rule 3 ─────────────────────────────────────────────────────────────────
  //
  // A production parameter that DEFAULTS an instant to the wall clock (`at: Instant =
  // Instant.now()`) lets a caller who holds the injected clock forget to pass it, and nothing
  // notices until two components judge one stamp on different clocks — the detail reaper and
  // handler disagreed about "fresh" exactly that way, and a harness pinned to 2026-06-08 then
  // re-asked for work a system-clock stamp said was done. A `clock: Clock = Clock.systemUTC()`
  // default is a SEAM, not a read, and stays allowed — as does a `() => Instant` function one.

  /** `file:parameter` → why that main-source parameter may still default to the wall clock. */
  private val MainDefaultAllowlist: Map[String, String] = Map(
    "common/src/main/scala/services/tasks/TaskQueue.scala:submittedAt" ->
      "queue bookkeeping (the task's submit stamp), read by no freshness decision; ~60 call sites — follow-up",
    "common/src/main/scala/services/tasks/TaskQueue.scala:now" ->
      "lease expiry for claim/reap, compared only with other queue stamps the same default wrote — follow-up",
    "common/src/main/scala/tools/RelativeTime.scala:reference" ->
      "renders \"3 minutes ago\" for an operator page at request time; nothing is decided from it")

  private val MainWallClockRead = """\b(?:Instant|LocalDate|LocalDateTime|ZonedDateTime|OffsetDateTime)\.now\(\s*\)|System\.currentTimeMillis\(""".r
  // A value default only: `now: () => Instant = () => Instant.now()` is a seam, like a Clock.
  private val ParameterName     = """(?:^\s*|[(,]\s*)(\w+)\s*:(?:=>|[^=,(])+=(?!>)\s*$""".r

  private def mainClockDefaults: Seq[(String, String)] =
    scalaFiles(ScalaSourceScan.MainRoots).flatMap { path =>
      read(path).linesIterator.zipWithIndex.flatMap { case (raw, index) =>
        val line = code(raw)
        MainWallClockRead.findAllMatchIn(line).flatMap { m =>
          val prefix = line.substring(0, m.start).replaceAll("""(?:java\.(?:time|lang)\.)$""", "")
          ParameterName.findFirstMatchIn(prefix).map(p => s"$path:${p.group(1)}" -> s"$path:${index + 1}: ${raw.trim}")
        }
      }
    }

  "Main sources" should "not default a parameter to a wall-clock READ — take the injected clock's instant instead" in {
    val offenders = mainClockDefaults.filterNot { case (key, _) => MainDefaultAllowlist.contains(key) }.map(_._2)
    withClue("These parameters default to the wall clock, so a caller holding the worker's clock can silently skip " +
      "it. Drop the default and pass `clock.instant()`, or allowlist the parameter with a reason:\n" +
      offenders.mkString("\n") + "\n") {
      offenders shouldBe empty
    }
  }

  it should "keep every allowlisted parameter still defaulting to the wall clock" in {
    val live = mainClockDefaults.map(_._1).toSet
    (MainDefaultAllowlist.keySet -- live) shouldBe empty
  }

  // ── rule 2 ─────────────────────────────────────────────────────────────────

  private val ClockDefault = """clock\s*:\s*(?:java\.time\.)?Clock\s*=\s*(?:java\.time\.)?Clock\.system""".r
  private val ClassHeader  = """\bclass\s+(\w+)\s*(?:\[[^\]]*\])?\s*(?=\()""".r

  /** Production classes whose constructor defaults `clock` to the system clock. */
  private lazy val clockDefaulted: Set[String] = scalaFiles(ScalaSourceScan.MainRoots).flatMap { path =>
    val src  = read(path).linesIterator.map(code).mkString("\n")
    ClassHeader.findAllMatchIn(src).collect {
      case m if ClockDefault.findFirstIn(argumentsAt(src, m.end)).isDefined => m.group(1)
    }.toSeq
  }.toSet

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

    clockDefaulted should contain ("CaffeineMovieCache")
    val constructions = clockDefaulted.toSeq.sorted.map(name => name -> s"""\\bnew\\s+$name\\b\\s*(?:\\[[^\\]]*\\])?\\s*\\(""".r)
    val offenders = held.toSeq.sortBy(_._1.toString).flatMap { case (path, why) =>
      val src = sources(path)
      constructions.flatMap { case (name, ctor) =>
        ctor.findAllMatchIn(src).collect {
          case m if !argumentsAt(src, m.end - 1).toLowerCase.contains("clock") =>
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

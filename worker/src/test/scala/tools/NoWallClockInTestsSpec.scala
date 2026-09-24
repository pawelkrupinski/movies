package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.time.{LocalDate, ZoneOffset}
import scala.jdk.CollectionConverters._

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
  private val MainRoots = Seq("common/src/main", "web/src/main", "worker/src/main")

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
    "web/src/test/scala/controllers/UserStateControllerSpec.scala" ->
      ("UserStateController stamps every change with Instant.now() (no Clock seam) and its newer-than rules " +
        "compare the stored stamps with it"),
    "web/src/test/scala/controllers/TasksControllerSpec.scala" ->
      "asserts the tasks page reports the server's current time (TasksController reads System.currentTimeMillis)",
    "worker/src/test/scala/services/tasks/TaskWorkerSpec.scala" ->
      "TaskWorker stamps a released task's retry back-off with Instant.now() (no Clock seam); claims are relative to it",
    "worker/src/test/scala/services/cinemas/ScriptedCinemaScraper.scala" ->
      "a filler showtime for scripted listings fed through caches on the system clock",
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
  ).map(spec => s"worker/src/it/scala/$spec.scala" -> MongoPath)

  private def scalaFiles(roots: Seq[String]): Seq[Path] =
    roots.map(Paths.get(_)).filter(Files.isDirectory(_)).flatMap { root =>
      Files.walk(root).iterator.asScala.filter(_.toString.endsWith(".scala")).toSeq
    }.sortBy(_.toString)

  private def read(p: Path): String = new String(Files.readAllBytes(p), StandardCharsets.UTF_8)

  /** The line with any `//` comment dropped, or "" for a Scaladoc/block-comment line. */
  private def code(line: String): String = {
    val trimmed = line.trim
    if (trimmed.startsWith("*") || trimmed.startsWith("/*")) ""
    else {
      val slashes = Iterator.iterate(line.indexOf("//"))(i => line.indexOf("//", i + 1))
        .takeWhile(_ >= 0)
        .find(i => line.substring(0, i).count(_ == '"') % 2 == 0)
      slashes.fold(line)(line.substring(0, _))
    }
  }

  /** The argument text of the call whose `(` is at `open`. */
  private def argumentsAt(text: String, open: Int): String = {
    var depth = 0
    var i     = open
    while (i < text.length) {
      text(i) match {
        case '(' => depth += 1
        case ')' =>
          depth -= 1
          if (depth == 0) return text.substring(open + 1, i)
        case _ =>
      }
      i += 1
    }
    text.substring(open + 1)
  }

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

  // ── rule 2 ─────────────────────────────────────────────────────────────────

  private val ClockDefault = """clock\s*:\s*(?:java\.time\.)?Clock\s*=\s*(?:java\.time\.)?Clock\.system""".r
  private val ClassHeader  = """\bclass\s+(\w+)\s*(?:\[[^\]]*\])?\s*(?=\()""".r

  /** Production classes whose constructor defaults `clock` to the system clock. */
  private lazy val clockDefaulted: Set[String] = scalaFiles(MainRoots).flatMap { path =>
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

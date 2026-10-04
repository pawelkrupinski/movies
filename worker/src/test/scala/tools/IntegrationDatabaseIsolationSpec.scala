package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}

/**
 * Every `it/` spec works in a database of its own.
 *
 * The it suites run in PARALLEL (`IntegrationTest / parallelExecution := true`, web and
 * worker side by side under `itAll`). Against the one shared `MONGODB_DB` they kept out of
 * each other's way by naming: sentinel id prefixes, and an `afterAll` that deletes "its"
 * rows by regex. A regex is only as narrow as the prefixes nobody else chose.
 * `UserRepositoryIntegrationSpec` purged `userStates` by `^__integration-test-`, which
 * also matched `HiddenFilmsConcurrentWritesIntegrationSpec`'s `__integration-test-hide-`
 * rows, and when that purge landed between the hidden-films writes and their read-back
 * every write was a 200 and the row was gone (main run 36157984407, "flaky").
 *
 * Three rules, each naming file:line:
 *
 *  1. No it/ source opens the SHARED database: no `"MONGODB_DB"` read, no
 *     `MongoAddress.fromEnv` (which carries it), no literal `getDatabase("kinowo")`, and no fixed
 *     `"kinowo_…"` name (one with no `nanoTime`/`pid()` in it), which every run on the same
 *     server shares. Take a database of the
 *     spec's own from `IsolatedMongoDatabase` or `IntegrationCorpusDatabase`
 *     (`<MONGODB_DB>_<suite>_<pid>`) — both unique per run — and drop it afterwards.
 *
 *  2. No `getDatabase` by a literal name, whatever its prefix, unless something unique to the run
 *     (`nanoTime`, `pid()`) is in it: two runs on one server would share it.
 *
 *  3. No delete by regex. In a database the spec owns, drop it; a pattern delete there is
 *     only allowed where the delete IS the scenario, and is allowlisted with that reason.
 */
class IntegrationDatabaseIsolationSpec extends AnyFlatSpec with Matchers {

  import ScalaSourceScan.{argumentsAt, code, codeOf, read, scalaFiles}

  private val ItRoots = Seq("web/src/it", "worker/src/it", "common/src/it", "e2e/src/it")

  /** file → why it may still name the shared database. */
  private val SharedDatabaseAllowlist: Map[String, String] = Map(
    "worker/src/it/scala/IntegrationCorpusDatabaseIntegrationSpec.scala" ->
      "reads the base MONGODB_DB only to assert the per-suite names derive from it; opens no database by it")

  /** file → why it may open a database by a literal name. */
  private val LiteralDatabaseAllowlist: Map[String, String] = Map(
    "worker/src/it/scala/ChangeStreamResumeTokenIntegrationSpec.scala" ->
      "the literal is on a client pointed at an unreachable address: no server ever sees the name",
    "worker/src/it/scala/FallbackStoreHydrateIntegrationSpec.scala" ->
      "the literal is on a client pointed at an unreachable address: no server ever sees the name",
    "worker/src/it/scala/ResolutionStoreClearIntegrationSpec.scala" ->
      "the literal is on a client pointed at an unreachable address: no server ever sees the name",
  )

  /** file → why a pattern delete is the scenario itself, in a database the spec owns. */
  private val RegexDeleteAllowlist: Map[String, String] = Map.empty

  // The shared database is the resolved MONGODB_DB — `IntegrationMongoTarget.databasePrefix`, which
  // every per-suite database name is derived from — or the raw key / literal name.
  // A FIXED database name ("kinowo_…") is shared too: by every run on the same server, so two
  // itAll runs side by side drop each other's database mid-test (seen 2026-10-04: "Cannot create
  // collection kinowo_it_wiring_de.database_owner - database is in the process of being dropped").
  private val SharedDatabase = """"MONGODB_DB"|\.databasePrefix\b|\bMongoAddress\s*\.\s*fromEnv\b|getDatabase\(\s*"kinowo"\s*\)""".r
  // `\bs?"` would need a word character before a plain `"`, so it caught only `s"kinowo_…"` and let
  // SharedUsersDatabaseIntegrationSpec's plain "kinowo_it_sharedusers" through. A line naming no
  // database (a `kinowo_worker_…` metric name) is not one.
  private val FixedName      = """(?:\bs)?"kinowo_""".r
  private val DatabaseWord   = """(?i)database|db\b|prefix""".r
  private val UniquePerRun   = """nanoTime|\bpid\(\)""".r
  // A database opened by a literal name: plain ("x"), or interpolated with nothing unique to the run in
  // it. `admin` is the server's own, never written by a spec.
  private val LiteralDatabase = """getDatabase\(\s*(?:"(?!admin")|s")""".r
  private val Delete         = """\.delete(?:Many|One)\s*\(""".r
  private val PatternFilter  = """Filters\.regex\(|\$regex|BsonRegularExpression|Pattern\.compile""".r

  private lazy val files: Seq[Path] = scalaFiles(ItRoots)

  private def sharedDatabaseLines(path: Path): Seq[String] =
    read(path).linesIterator.zipWithIndex.collect {
      case (line, index) if SharedDatabase.findFirstIn(code(line)).isDefined ||
          (FixedName.findFirstIn(code(line)).isDefined && DatabaseWord.findFirstIn(code(line)).isDefined &&
            UniquePerRun.findFirstIn(code(line)).isEmpty) =>
        s"$path:${index + 1}: ${line.trim}"
    }.toSeq

  private def literalDatabaseLines(path: Path): Seq[String] =
    read(path).linesIterator.zipWithIndex.collect {
      case (line, index) if LiteralDatabase.findFirstIn(code(line)).isDefined && UniquePerRun.findFirstIn(code(line)).isEmpty =>
        s"$path:${index + 1}: ${line.trim}"
    }.toSeq

  private def regexDeletes(path: Path): Seq[String] = {
    val src = codeOf(path)
    Delete.findAllMatchIn(src).collect {
      case m if PatternFilter.findFirstIn(argumentsAt(src, m.end - 1)).isDefined =>
        val line = src.substring(0, m.start).count(_ == '\n')
        s"$path:${line + 1}: ${src.linesIterator.drop(line).next().trim}"
    }.toSeq
  }

  "it/ specs" should "not open the shared integration database" in {
    files.nonEmpty shouldBe true
    val offenders = files.filterNot(p => SharedDatabaseAllowlist.contains(p.toString)).flatMap(sharedDatabaseLines)
    withClue(
      "These it/ lines open the database every parallel suite shares. Use IsolatedMongoDatabase.open / withDatabase " +
        "or IntegrationCorpusDatabase.withDatabase and drop it afterwards, or allowlist the file with a reason:\n" +
        offenders.mkString("\n") + "\n") {
      offenders shouldBe empty
    }
  }

  they should "open no database by a name fixed across runs" in {
    val offenders = files.filterNot(p => LiteralDatabaseAllowlist.contains(p.toString)).flatMap(literalDatabaseLines)
    withClue(
      "These it/ lines open a database whose name every run on the same server shares, so two runs drop each " +
        "other's data. Take it from IsolatedMongoDatabase or IntegrationCorpusDatabase (both unique per run), or " +
        "allowlist the file with a reason:\n" + offenders.mkString("\n") + "\n") {
      offenders shouldBe empty
    }
  }

  they should "not delete by regex or prefix" in {
    val offenders = files.filterNot(p => RegexDeleteAllowlist.contains(p.toString)).flatMap(regexDeletes)
    withClue(
      "These it/ lines delete by pattern — in a shared database that reaches rows another suite named alike. " +
        "Drop the spec's own database instead, or allowlist the file with a reason:\n" + offenders.mkString("\n") + "\n") {
      offenders shouldBe empty
    }
  }

  it should "keep every allowlist entry pointing at a file that still needs it" in {
    def stale(allowlist: Map[String, String], still: Path => Seq[String]) =
      allowlist.keys.toSeq.sorted.filterNot { file =>
        val path = Paths.get(file)
        Files.exists(path) && still(path).nonEmpty
      }
    withClue("Allowlisted but no longer offending — drop the entry:\n") {
      (stale(SharedDatabaseAllowlist, sharedDatabaseLines) ++ stale(RegexDeleteAllowlist, regexDeletes) ++
        stale(LiteralDatabaseAllowlist, literalDatabaseLines)) shouldBe empty
    }
  }
}

package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import ScalaSourceScan.{code, read, scalaFiles}

/**
 * Every pool a composition root builds is registered with its [[ManagedResources]], so `stop()` shuts it.
 *
 * The class of bug: a wiring member that builds an executor or scheduler which `stop()` never names —
 * the identity model's scheduler and prefetch pool, a trace store per rebuild, the debug dashboard's
 * pools, the scrape's adaptive-timeout pool (never shut at all) — each found one at a time. In the
 * wiring sources (each `…Wiring.scala` under `modules`), a `DaemonExecutors.…(` call must sit inside a
 * `managedResources.executor(…)` / `managedResources.register(…)` in the same member definition.
 * Where a pool is rightly owned elsewhere, add it to [[Allowlist]] with WHY.
 */
class NoUnmanagedWiringExecutorSpec extends AnyFlatSpec with Matchers {

  private val WiringRoots = Seq("worker/src/main/scala/modules", "web/src/main/scala/modules")
  private val Pool        = """\bDaemonExecutors\s*\.\s*\w+\s*\(""".r
  private val Member      = """^\s*(?:(?:private|protected)(?:\[\w+\])?\s+|override\s+|final\s+|implicit\s+)*(?:lazy\s+val|val|def)\s""".r
  private val Registered  = """\bmanagedResources\s*\.\s*(?:executor|register)\s*[(\[]""".r

  /** (repository-relative file, the flagged line trimmed) → why the pool is not the wiring's to shut. */
  private val Allowlist: Map[(String, String), String] = Map.empty

  private[tools] def offenders(source: String): Seq[(Int, String)] = {
    val lines = source.linesIterator.map(code).toVector
    lines.indices.flatMap { i =>
      Pool.findFirstMatchIn(lines(i)).flatMap { hit =>
        val start  = (i to 0 by -1).find(j => Member.findFirstIn(lines(j)).isDefined).getOrElse(0)
        val member = (lines.slice(start, i) :+ lines(i).substring(0, hit.start)).mkString("\n")
        Option.when(Registered.findFirstIn(member).isEmpty)(i + 1 -> lines(i).trim)
      }
    }
  }

  private lazy val found: Seq[(String, Int, String)] = for {
    file         <- scalaFiles(WiringRoots).filter(_.getFileName.toString.endsWith("Wiring.scala"))
    (line, text) <- offenders(read(file))
  } yield (file.toString, line, text)

  "The wiring sources" should "register every pool they build with managedResources, outside the allowlist" in {
    val unexplained = found.filterNot { case (file, _, text) => Allowlist.contains(file -> text) }
    withClue(unexplained.map { case (file, line, text) => s"$file:$line: $text" }.mkString("\n", "\n", "\n"))(unexplained shouldBe empty)
  }

  "The allowlist" should "name only sites that still exist" in {
    (Allowlist.keySet -- found.map { case (file, _, text) => file -> text }.toSet) shouldBe empty
  }

  "The lint" should "flag a pool built outside a registration, in the same member or across its lines" in {
    val src =
      """lazy val a = DaemonExecutors.scheduler("a")
        |lazy val b = managedResources.executor("b")(DaemonExecutors.scheduler("b"))
        |protected lazy val c: ExecutorService =
        |  managedResources.executor("c")(
        |    DaemonExecutors.virtualThreadEC("c"))
        |protected lazy val d: ExecutorService =
        |  DaemonExecutors.virtualThreadEC("d")
        |// DaemonExecutors.scheduler("in a comment")
        |""".stripMargin
    offenders(src).map(_._1) shouldBe Seq(1, 7)
  }
}

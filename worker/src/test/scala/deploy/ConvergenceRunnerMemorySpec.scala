package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Keeps the convergence legs inside the RUNNER, not just inside their own `-Xmx`.
 *
 * A leg runs two memory-hungry processes on one `ubuntu-24.04` box: the sbt JVM
 * holding a whole country resident, and a mongod holding that country's databases.
 * Both default to sizing themselves as if they owned the machine — mongod's
 * WiredTiger cache is half of (RAM - 1GB), about 7.5 GB of the runner's 16 — so the
 * two defaults together oversubscribe it, and the largest country's heap could not
 * be raised at all while that was true.
 *
 * That was not a theoretical ceiling. The US order-independence row exited 3 on
 * `-XX:+ExitOnOutOfMemoryError` on 2026-09-05 with its assertion unreported, and the
 * obvious remedy — give it more heap — was the one thing unavailable, because there
 * was no headroom to give it from.
 *
 * So the rule is arithmetic over the whole box rather than a number per process. It
 * is here because no running-JVM layer can reach it: the only other way to discover
 * that a heap raise does not fit is to spend five hours of CI finding out.
 */
class ConvergenceRunnerMemorySpec extends AnyFlatSpec with Matchers {

  private lazy val setup    = RepoFile.read(".github/actions/convergence-setup/action.yml")
  private lazy val leg      = RepoFile.read(".github/workflows/country-convergence-leg.yml")
  private lazy val callers  = Seq(".github/workflows/identity-model-convergence.yml").map(RepoFile.read)

  /** `ubuntu-24.04`, which every convergence job runs on. */
  private val RunnerGb = 16

  /** What the box owes everything that is neither the JVM heap nor the WiredTiger
   *  cache: the JVM's own metaspace, code cache, thread stacks and direct buffers,
   *  mongod outside its cache, the sbt launcher, the checkout and the OS. Deliberately
   *  generous — the cost of being wrong here is a five-hour run that dies at the end. */
  private val OverheadGb = 3

  private def gigabytes(pattern: String, text: String): Seq[Int] =
    pattern.r.findAllMatchIn(text).map(_.group(1).toInt).toSeq

  /** The cache a row names (`mongo-cache-gb`), else the setup's fallback (`--wiredTigerCacheSizeGB ${{ inputs.mongo-cache-gb || 2 }}`). */
  private lazy val defaultCacheGb: Seq[Int] =
    gigabytes("""--wiredTigerCacheSizeGB\s+\$\{\{\s*inputs\.mongo-cache-gb\s*\|\|\s*(\d+)\s*}}""", setup)
  private def cacheGb(row: Map[String, String]): Int = row.get("mongo-cache-gb").map(_.toInt).getOrElse(defaultCacheGb.max)
  private lazy val wiredTigerGb: Seq[Int] = defaultCacheGb ++ callers.flatMap(RepoFile.matrixRows).map(cacheGb)
  private lazy val heapGb: Seq[Int] = callers.flatMap(gigabytes("""heap:\s*(\d+)g""", _))

  "the convergence runner" should "cap mongod's WiredTiger cache rather than let it size for the whole box" in {
    withClue("`docker run … mongo` in convergence-setup must name --wiredTigerCacheSizeGB; " +
             "mongod otherwise takes ~7.5GB of a 16GB runner the sbt JVM is sharing:\n") {
      wiredTigerGb should not be empty
    }
  }

  it should "leave each country's heap, its cache and the overhead inside the runner" in {
    heapGb should not be empty
    callers.flatMap(RepoFile.matrixRows).foreach { row =>
      val heap  = row("heap").stripSuffix("g").toInt
      val cache = cacheGb(row)
      withClue(s"${row("country")}: heap ${heap}g + WiredTiger ${cache}g + ${OverheadGb}g overhead must fit ${RunnerGb}g; " +
               "raise one and something else has to give:\n") {
        heap + cache + OverheadGb should be <= RunnerGb
      }
    }
  }

  it should "apply the declared heap to the order-independence run as well as the full one" in {
    // The row that died had its own sbt invocation. A `-Xmx` threaded into only one of
    // the leg's two commands would leave the other on `.jvmopts`' 4g default.
    val invocations = """sbt -J-Xmx\$\{\{ inputs\.heap \}\}""".r.findAllIn(leg).size
    invocations should be >= 2
  }

  it should "fit each row's RAM-backed Mongo beside its own heap, the cache and the overhead" in {
    // tmpfs pages are memory, so a row that holds Mongo's data in RAM spends its size from the same 16g.
    val rows = callers.flatMap(RepoFile.matrixRows)
    val held = rows.filter(_.contains("mongo-tmpfs"))
    held should not be empty
    held.foreach { row =>
      val heap  = row("heap").stripSuffix("g").toInt
      val tmpfs = row("mongo-tmpfs").stripSuffix("g").toInt
      withClue(s"${row("country")}: heap ${heap}g + tmpfs ${tmpfs}g + WiredTiger ${cacheGb(row)}g + ${OverheadGb}g overhead must fit ${RunnerGb}g:\n") {
        heap + tmpfs + cacheGb(row) + OverheadGb should be <= RunnerGb
      }
    }
  }
}

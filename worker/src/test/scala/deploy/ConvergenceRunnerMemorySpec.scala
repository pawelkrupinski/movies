package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Keeps the convergence legs inside the RUNNER, not just inside their own `-Xmx`.
 *
 * A leg runs two memory-hungry processes on one `ubuntu-latest` box: the sbt JVM
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
  private lazy val callers  = Seq(".github/workflows/country-convergence.yml",
                                  ".github/workflows/us-convergence.yml").map(RepoFile.read)

  /** `ubuntu-latest`, which every convergence job runs on. */
  private val RunnerGb = 16

  /** What the box owes everything that is neither the JVM heap nor the WiredTiger
   *  cache: the JVM's own metaspace, code cache, thread stacks and direct buffers,
   *  mongod outside its cache, the sbt launcher, the checkout and the OS. Deliberately
   *  generous — the cost of being wrong here is a five-hour run that dies at the end. */
  private val OverheadGb = 3

  private def gigabytes(pattern: String, text: String): Seq[Int] =
    pattern.r.findAllMatchIn(text).map(_.group(1).toInt).toSeq

  private lazy val wiredTigerGb: Seq[Int] = gigabytes("""--wiredTigerCacheSizeGB\s+(\d+)""", setup)
  private lazy val heapGb: Seq[Int] = callers.flatMap(gigabytes("""heap:\s*(\d+)g""", _))

  "the convergence runner" should "cap mongod's WiredTiger cache rather than let it size for the whole box" in {
    withClue("`docker run … mongo:7` in convergence-setup must name --wiredTigerCacheSizeGB; " +
             "mongod otherwise takes ~7.5GB of a 16GB runner the sbt JVM is sharing:\n") {
      wiredTigerGb should not be empty
    }
  }

  it should "leave the largest country's heap, the cache and the overhead inside the runner" in {
    val heap  = heapGb.max
    val cache = wiredTigerGb.max
    withClue(s"heap ${heap}g + WiredTiger ${cache}g + ${OverheadGb}g overhead must fit ${RunnerGb}g; " +
             "raise one and something else has to give:\n") {
      heap + cache + OverheadGb should be <= RunnerGb
    }
  }

  it should "apply the declared heap to the order-independence run as well as the full one" in {
    // The row that died had its own sbt invocation. A `-Xmx` threaded into only one of
    // the leg's two commands would leave the other on `.jvmopts`' 4g default.
    val invocations = """sbt -J-Xmx\$\{\{ inputs\.heap \}\}""".r.findAllIn(leg).size
    invocations should be >= 2
  }
}

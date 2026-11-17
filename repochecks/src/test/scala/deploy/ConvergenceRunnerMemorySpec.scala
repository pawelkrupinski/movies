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
   *  mongod outside its cache, the sbt launcher, the checkout and the OS. MEASURED, not guessed
   *  (scripts/ci/cgroup-sampler.sh in every row, US runs 1258/1259/1276, 2026-10-22): the 10g JVM's RSS
   *  peaked at 12.15 GB (1.4 GB beyond its heap), mongod at 0.7-0.8 GB beyond its cache, and nothing
   *  else in the job's cgroup held more than ~0.1 GB -- 2.2 GB, so 2.5 of margin. It was 3 when it was a guess. */
  private val OverheadGb = 2.5

  /** Each caller's matrix rows, with their values as written (quotes dropped): `0.5` and `'false'` included,
   *  which RepoFile.matrixRows' word-only values cut short. */
  private lazy val rows: Seq[Map[String, String]] = callers.flatMap { yaml =>
    """-\s*\{\s*country:([^}]*)}""".r.findAllMatchIn(yaml).map { row =>
      ("country:" + row.group(1)).split(",").toSeq.flatMap { field =>
        field.split(":", 2) match {
          case Array(k, v) => Some(k.trim -> v.trim.stripPrefix("'").stripSuffix("'"))
          case _           => None
        }
      }.toMap
    }
  }

  /** The setup's fallback cache (`--wiredTigerCacheSizeGB ${{ inputs.mongo-cache-gb || 2 }}`). */
  private lazy val defaultCacheGb: Seq[Double] =
    """--wiredTigerCacheSizeGB\s+\$\{\{\s*inputs\.mongo-cache-gb\s*\|\|\s*([\d.]+)\s*}}""".r
      .findAllMatchIn(setup).map(_.group(1).toDouble).toSeq
  private def gb(value: String): Double = value.stripSuffix("g").toDouble
  /** A row's cache where its Mongo is on a tmpfs, and on disk: an order row kept on disk (`mongo-tmpfs-order: 'false'`)
   *  takes the setup's fallback, as country-convergence-leg.yml hands it. */
  private def tmpfsCacheGb(row: Map[String, String]): Double = row.get("mongo-cache-gb").map(gb).getOrElse(defaultCacheGb.max)
  private def diskCacheGb(row: Map[String, String]): Double =
    if (row.get("mongo-tmpfs-order").contains("false")) tmpfsCacheGb(row) max defaultCacheGb.max else tmpfsCacheGb(row)
  private lazy val wiredTigerGb: Seq[Double] = defaultCacheGb ++ rows.map(tmpfsCacheGb)

  // Germany's recording holds the boot AND its three order passes in one JVM (`order: ''`, the whole `convergenceGermany`),
  // where a hermetic leg splits them into rows: at 4g it thrashed in GC and died in the passes' first projection (runs 322,
  // 408, and Record scrape fixtures #168, an override of 8g having been left empty since Forgejo).
  "Germany's recording" should "have the heap its whole boot and order passes need, and fit the runner with it" in {
    val recording = RepoFile.read(".github/workflows/record-scrape-fixtures.yml")
    val heap = """country: germany,[^}]*\bheap: (\d+g)""".r.findFirstMatchIn(recording).map(_.group(1)).getOrElse(fail("no germany row"))
    gb(heap) should be >= 8.0
    gb(heap) + defaultCacheGb.max + OverheadGb should be <= RunnerGb.toDouble
    withClue("the row's heap reaches the leg as written: ")(recording should include ("heap: ${{ matrix.heap }}"))
  }

  "the convergence runner" should "cap mongod's WiredTiger cache rather than let it size for the whole box" in {
    withClue("`docker run … mongo` in convergence-setup must name --wiredTigerCacheSizeGB; " +
             "mongod otherwise takes ~7.5GB of a 16GB runner the sbt JVM is sharing:\n") {
      wiredTigerGb should not be empty
    }
  }

  it should "leave each country's heap, its cache and the overhead inside the runner" in {
    rows should not be empty
    rows.foreach { row =>
      val heap  = gb(row("heap"))
      val cache = diskCacheGb(row)
      withClue(s"${row("country")}: heap ${heap}g + WiredTiger ${cache}g + ${OverheadGb}g overhead must fit ${RunnerGb}g; " +
               "raise one and something else has to give:\n") {
        heap + cache + OverheadGb should be <= RunnerGb.toDouble
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
    val held = rows.filter(_.get("mongo-tmpfs").exists(_.nonEmpty))
    held should not be empty
    held.foreach { row =>
      val heap  = gb(row("heap"))
      val tmpfs = gb(row("mongo-tmpfs"))
      val cache = tmpfsCacheGb(row)
      withClue(s"${row("country")}: heap ${heap}g + tmpfs ${tmpfs}g + WiredTiger ${cache}g + ${OverheadGb}g overhead must fit ${RunnerGb}g:\n") {
        heap + tmpfs + cache + OverheadGb should be <= RunnerGb.toDouble
      }
    }
  }

  it should "keep an order row on disk when its country says so" in {
    // The United States' order rows: on a tmpfs they left 1.6 GB of the cap free, on disk 2.2 (runs 1276, 1258).
    rows.find(_("country") == "united-states").flatMap(_.get("mongo-tmpfs-order")) shouldBe Some("false")
    leg should include ("inputs.mongo-tmpfs-order != 'false' || !startsWith(matrix.phase, 'order-independence')")
  }
}

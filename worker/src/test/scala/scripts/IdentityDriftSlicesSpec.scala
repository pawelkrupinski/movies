package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * TIER 1 OF THE DRIFT GATE: every listing that ever diverged or flipped in the incremental = whole replay
 * (scripts/ci/identity-drift-check.sh, the convergence legs' `takes-drift-<seed>` rows) is a committed drift slice —
 * `test/resources/fixtures/drift-slices/<cc>/<name>/`, cut by `scripts/identity-slice <cc> SELECTOR --emit-drift-slice`:
 * the listing, its families and corpus context, and every answer the incremental sequence reads. Each is replayed
 * here through production's incremental sequence ([[IdentityDrift.SliceRun]]: boot with answers missing, ticks,
 * re-scrapes, leave and return, two deploys through BSON) for [[Seeds]] seeds, from its own answers alone, and fails on
 * any divergence from the whole resolve or any flip the two allow-lists do not name — the CI gate's checks, in minutes
 * on the laptop. A drift or flip red is not closed until its slice is committed here (docs/landing-process.md).
 */
class IdentityDriftSlicesSpec extends AnyFlatSpec with Matchers {
  import IdentityDriftSlicesSpec._

  private val fixtures = Path.of("test", "resources", "fixtures")
  private val allowed  = IdentityDrift.allowList(fixtures.resolve("identity-drift-allow.tsv"))
  private val flipsOk  = IdentityDrift.flipAllowList(fixtures.resolve("identity-flips-allow.tsv"))
  private val slices   = sliceDirs(fixtures.resolve("drift-slices"))

  /** Every slice's findings, the slices side by side: each replays its own answers and shares nothing with another. */
  private lazy val found: Map[Path, Try[Outcome]] = {
    val pool = java.util.concurrent.Executors.newFixedThreadPool(math.max(1, Runtime.getRuntime.availableProcessors / 2))
    try slices.zip(ParallelTasks.all(slices)(dir => Try(replayed(dir)))(using
      scala.concurrent.ExecutionContext.fromExecutor(pool))).toMap
    finally pool.shutdown()
  }

  "the drift slices" should "be there: one per listing that ever diverged or flipped" in {
    slices should not be empty
  }

  slices.foreach { dir =>
    "A drift slice" should s"replay incremental = whole with no divergence nor flip beyond the allow-lists: ${fixtures.relativize(dir)}" in {
      val outcome = found(dir).get
      // a request the slice does not hold is read as unknown, as a leg reads a gap of its recording: code that asks
      // something new still replays the slice, deterministically; cutting it again brings the answer in
      if (outcome.lacking.nonEmpty) info(s"${outcome.lacking.size} request(s) the slice lacks, read as unknown (scripts/identity-slice --emit-drift-slice cuts it again)")
      val open = outcome.findings.open(outcome.country, allowed, flipsOk)
      withClue("divergences from the whole resolve: ")(open.divergences.map(d => s"${d.key.venue} | ${d.key.rawTitle}: whole ${d.whole.film}, " +
        s"incremental ${d.incremental.film} [${d.checkpoint}, seed ${d.seed}]").distinct shouldBe empty)
      withClue("flips: ")(open.flips.map { case (seed, f) => s"${f.line} [seed $seed]" }.distinct shouldBe empty)
    }
  }
}

object IdentityDriftSlicesSpec {

  /** How many seeds each slice replays: each one a different share of the answers held at boot, a different arrival and
   *  re-scrape order — cheap at a slice's size. */
  val Seeds: Seq[Int] = 1 to 12

  /** The share of answers held at boot, as the CI gate's default. */
  val Gate: Int = IdentityDrift.Options().gate

  final case class Outcome(country: String, findings: IdentityDrift.Findings, lacking: Seq[String])

  /** Every directory under `root` holding a slice. */
  def sliceDirs(root: Path): Seq[Path] =
    if (!Files.isDirectory(root)) Nil
    else Files.walk(root).iterator().asScala.filter(d => Files.exists(d.resolve("slice.json.gz"))).toSeq.sortBy(_.toString)

  def replayed(dir: Path): Outcome = {
    val run = new IdentityDrift.SliceRun(dir)
    Outcome(run.slice.country.code, quietly(run.run(Seeds, Gate)), run.lacking)
  }

  /** The replay's progress lines, kept off the suite's output. */
  private def quietly[A](body: => A): A = {
    val sink = new java.io.PrintStream(java.io.OutputStream.nullOutputStream())
    Console.withOut(sink)(Console.withErr(sink)(body))
  }
}

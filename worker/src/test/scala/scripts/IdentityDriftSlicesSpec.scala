package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * TIER 1 OF THE DRIFT GATE: every listing that ever diverged or flipped in the incremental = whole replay
 * (scripts/ci/identity-drift-check.sh, the convergence legs' `takes-drift-1|2` rows, each on its run's rotating seed) is a committed drift slice —
 * `test/resources/fixtures/drift-slices/<cc>/<name>/`, cut by `scripts/identity-slice <cc> SELECTOR --emit-drift-slice`:
 * the listing, its families and corpus context, and every answer the incremental sequence reads. Each is replayed
 * here through production's incremental sequence ([[IdentityDrift.SliceRun]]: boot with answers missing, ticks,
 * re-scrapes, leave and return, two deploys through BSON) for [[Seeds]] seeds, from its own answers alone, and fails on
 * any divergence from the whole resolve or any flip the two allow-lists do not name — the CI gate's checks, in minutes
 * on the laptop. A drift or flip red is not closed until its slice is committed here (docs/landing-process.md).
 */
abstract class IdentityDriftSlicesShard(shard: Int) extends AnyFlatSpec with Matchers {
  import IdentityDriftSlicesSpec._

  protected val fixtures = Fixtures
  private val allowed  = IdentityDrift.allowList(fixtures.resolve("identity-drift-allow.tsv"))
  private val flipsOk  = IdentityDrift.flipAllowList(fixtures.resolve("identity-flips-allow.tsv"))
  /** The known flips (identity-flips-baseline.tsv): a slice of a baselined listing may flip, on any seed, while its row
   *  stands -- the corpus gate's own rule -- and is held strictly once the fix removes it. A tightened check's birth
   *  baselines what it newly sees (a229501a7's precise none: CinemaxX "Titans" goes to no film and back). */
  private val baselined = IdentityDrift.flipBaseline(fixtures.resolve("identity-flips-baseline.tsv"))
  private val parts    = shards(AllSlices, costs(CostsFile))(shard)

  /** This shard's parts' findings, side by side: each replays its own answers and shares nothing with another. */
  private lazy val found: Map[Part, Try[Outcome]] = {
    val pool = java.util.concurrent.Executors.newFixedThreadPool(math.max(1, Runtime.getRuntime.availableProcessors / 2))
    try parts.zip(ParallelTasks.all(parts)(part => Try(replayed(part.dir, part.seeds)))(using
      scala.concurrent.ExecutionContext.fromExecutor(pool))).toMap
    finally pool.shutdown()
  }

  parts.foreach { part =>
    val dir = part.dir
    "A drift slice" should s"replay incremental = whole with no divergence nor flip beyond the allow-lists: ${fixtures.relativize(dir)}" +
      (if (part.whole) "" else s" (seeds ${part.seeds.mkString(",")})") in {
      val outcome = found(part).get
      // what the part costs, for drift-slice-costs.tsv (a slice's cost is its parts' sum): its replay thread's CPU, and wall
      info(f"cost: ${sliceKey(dir)}%s ${outcome.cpuSeconds}%.1f s CPU, ${outcome.wallSeconds}%.1f s wall, seeds ${part.seeds.mkString(",")}%s")
      // a request the slice does not hold is read as unknown, as a leg reads a gap of its recording: code that asks
      // something new still replays the slice, deterministically; cutting it again brings the answer in
      if (outcome.lacking.nonEmpty) info(s"${outcome.lacking.size} request(s) the slice lacks, read as unknown (scripts/identity-slice --emit-drift-slice cuts it again)")
      val open = outcome.findings.open(outcome.country, allowed, flipsOk)
      // each red names its slice and seed, and the one line that replays just that (seconds)
      def again(seed: Long) = s"reproduce: ${reproducer(outcome.country, fixtures.relativize(dir), seed)}"
      withClue("divergences from the whole resolve: ")(open.divergences.map(d => s"${d.key.venue} | ${d.key.rawTitle}: whole ${d.whole.film}, " +
        s"incremental ${d.incremental.film} [${d.checkpoint}, seed ${d.seed}; ${again(d.seed)}]").distinct shouldBe empty)
      withClue("flips: ")(open.flips.filterNot { case (_, f) => baselined.exists(_.covers(outcome.country, f)) }
        .map { case (seed, f) => s"${f.line} [seed $seed; ${again(seed.toLong)}]" }.distinct shouldBe empty)
    }
  }
}

/**
 * The drift slices, SHARDED (2026-11-01: the whole set on one row, `test (worker-identity)`, took 420-580 s and grew with
 * every closed drift red, Main's long pole). [[IdentityDriftSlicesSpec.ShardCount]] suites each replay the slices
 * [[IdentityDriftSlicesSpec.shards]] gives them, balanced by each slice's measured cost (drift-slice-costs.tsv), and
 * build.sbt's rows put each on its own CI row: shard 0 (this suite, with the checks on the split itself) on
 * `worker-identity`, [[IdentityDriftSlicesShard1Spec]] on `heavy1`, [[IdentityDriftSlicesShard2Spec]] on `heavy2`,
 * [[IdentityDriftSlicesShard3Spec]] on `worker` (build.sbt `WorkerRestSuites`). Every
 * slice is replayed by exactly one shard, on the same seeds: the coverage of the one suite it was.
 */
class IdentityDriftSlicesSpec extends IdentityDriftSlicesShard(0) {
  import IdentityDriftSlicesSpec._

  "the drift slices" should "be there: one per listing that ever diverged or flipped" in {
    AllSlices should not be empty
  }

  they should "each be replayed on each of its seeds by exactly one shard" in {
    val split = shards(AllSlices, costs(CostsFile))
    split should have size ShardCount
    val replays = split.flatten.flatMap(p => p.seeds.map(p.dir.toString -> _))
    replays.sorted shouldBe AllSlices.flatMap(d => seedsOf(d).map(d.toString -> _)).sorted
    replays.distinct should have size replays.size.toLong
  }

  "The shards" should "be balanced by measured cost, heaviest first onto the lightest shard, an unmeasured slice at the median" in {
    val dirs = Seq("a", "b", "c", "d", "e").map(n => Path.of("drift-slices", "pl", n))
    val cost = Map("pl/a" -> 10.0, "pl/b" -> 6.0, "pl/c" -> 5.0, "pl/d" -> 4.0)
    // a: 10 of 31 is over half a shard's even share (7.75), so it is replayed in two parts of its seeds, 5 each
    shards(dirs, cost, 2).map(_.map(p => p.dir.getFileName.toString -> p.seeds.head)) shouldBe
      Seq(Seq("a" -> 1, "b" -> 1, "c" -> 1), Seq("a" -> 2, "d" -> 1, "e" -> 1))
    shards(dirs, cost, 2).flatten.filter(_.dir.getFileName.toString == "a").map(_.seeds) shouldBe
      Seq(Seeds.filter(_ % 2 == 1), Seeds.filter(_ % 2 == 0))
    shards(dirs, Map.empty, 2).map(_.size) shouldBe Seq(3, 2)
    shards(dirs, Map.empty, 2).flatten.forall(_.whole) shouldBe true
  }

  it should "read the costs file, one `<cc>/<name>  seconds` a line" in {
    val dir  = tools.TempDirectories.create("drift-slice-costs")
    val file = dir.resolve("costs.tsv")
    Files.writeString(file, "# measured on Main\npl/a\t12.5\n\nde/b\t3\n")
    costs(file) shouldBe Map("pl/a" -> 12.5, "de/b" -> 3.0)
    costs(dir.resolve("absent.tsv")) shouldBe empty
  }

  "A drift slice's seeds" should "be the twelve every slice replays, and the red seeds beyond them its seeds file names" in {
    val dir = tools.TempDirectories.create("drift-slice-seeds")
    seedsOf(dir) shouldBe Seeds
    Files.writeString(dir.resolve("seeds"), "# found red on convergence #549\n29\n3\n")
    seedsOf(dir) shouldBe Seeds :+ 29
  }
}

object IdentityDriftSlicesSpec {

  val Fixtures: Path = Path.of("test", "resources", "fixtures")

  /** Every committed slice. */
  lazy val AllSlices: Seq[Path] = sliceDirs(Fixtures.resolve("drift-slices"))

  /** How many suites the slices are split over: one per CI row that carries a shard (build.sbt `HeavyRows`,
   *  `WorkerRestSuites`, the `identity` row). Four, measured on Main run 41348471000: the slices' CPU was 1,157 s, a
   *  third of it one slice (Torrington "Dracula", 332 s), which alone held its row at 8.5 min (run 41350900273) until
   *  [[shards]] split a heavy slice's seeds into parts. */
  val ShardCount: Int = 4

  /** Each slice's measured replay cost, seconds of its replay thread's CPU on a Main runner (`cost:` in this spec's
   *  output; refresh it when slices land): what [[shards]] balances. */
  val CostsFile: Path = Fixtures.resolve("drift-slice-costs.tsv")

  /** A slice's key in [[CostsFile]]: `<cc>/<name>`. */
  def sliceKey(dir: Path): String = s"${dir.getParent.getFileName}/${dir.getFileName}"

  /** `file`'s costs, `<cc>/<name>` tab seconds a line (`#` comments); none when it is absent. */
  def costs(file: Path): Map[String, Double] =
    if (!Files.exists(file)) Map.empty
    else Files.readAllLines(file).asScala.toSeq.map(_.trim).filter(l => l.nonEmpty && !l.startsWith("#")).map { line =>
      val Array(key, seconds) = line.split("\\s+", 2)
      key -> seconds.trim.toDouble
    }.toMap

  /** `dirs` split into `count` shards by cost, longest-processing-time first: heaviest part first (ties by path, seed),
   *  each onto the shard carrying the least so far (ties the lowest). A slice with no measured cost counts the median
   *  of the measured ones (1 when none are). Deterministic: every JVM computes the same split. Each shard sorted by path. */
  def shards(dirs: Seq[Path], cost: Map[String, Double], count: Int = ShardCount): Seq[Seq[Part]] = {
    val known  = dirs.flatMap(d => cost.get(sliceKey(d))).sorted
    val median = if (known.isEmpty) 1.0 else known(known.size / 2)
    def of(d: Path) = cost.getOrElse(sliceKey(d), median)
    // A slice over half a shard's even share is replayed in parts, its seeds dealt round: one slice (Torrington
    // "Dracula", 332 s of CPU) was otherwise a row's floor on its own. Its seeds are independent replays.
    val cap   = dirs.map(of).sum / (2 * count)
    val parts = dirs.flatMap { d =>
      val seeds = seedsOf(d)
      val n     = math.max(1, math.min(seeds.size, math.ceil(of(d) / cap).toInt))
      (0 until n).map(j => Part(d, seeds.zipWithIndex.collect { case (s, i) if i % n == j => s }, of(d) / n, n == 1))
    }
    val load  = Array.fill(count)(0.0)
    val taken = Array.fill(count)(Vector.empty[Part])
    parts.sortBy(p => (-p.cost, p.dir.toString, p.seeds.head)).foreach { p =>
      val i = load.indices.minBy(i => (load(i), i))
      load(i) += p.cost
      taken(i) = taken(i) :+ p
    }
    taken.toSeq.map(_.sortBy(p => (p.dir.toString, p.seeds.head)))
  }

  /** What one shard's test replays: a slice on `seeds` -- all of its seeds (`whole`), or its share when it is split. */
  final case class Part(dir: Path, seeds: Seq[Int], cost: Double, whole: Boolean)

  /** How many seeds each slice replays: each one a different share of the answers held at boot, a different arrival and
   *  re-scrape order — cheap at a slice's size. */
  val Seeds: Seq[Int] = 1 to 12

  /** The share of answers held at boot, as the CI gate's default. */
  val Gate: Int = IdentityDrift.Options().gate

  final case class Outcome(country: String, findings: IdentityDrift.Findings, lacking: Seq[String], cpuSeconds: Double,
                           wallSeconds: Double)

  /** The one line that replays one slice on one seed: what a red of this spec names. */
  def reproducer(country: String, slice: Path, seed: Long): String =
    s"""sbt "worker/Test/runMain scripts.IdentityDrift $country --slice test/resources/fixtures/$slice --seed $seed""""

  /** Every directory under `root` holding a slice. */
  def sliceDirs(root: Path): Seq[Path] =
    if (!Files.isDirectory(root)) Nil
    else Files.walk(root).iterator().asScala.filter(d => Files.exists(d.resolve("slice.json.gz"))).toSeq.sortBy(_.toString)

  def replayed(dir: Path, seeds: Seq[Int]): Outcome = {
    val threads = java.lang.management.ManagementFactory.getThreadMXBean
    val (cpu0, wall0) = (threads.getCurrentThreadCpuTime, System.nanoTime)
    val run      = new IdentityDrift.SliceRun(dir)
    val findings = quietly(run.run(seeds, Gate))
    Outcome(run.slice.country.code, findings, run.lacking, (threads.getCurrentThreadCpuTime - cpu0) / 1e9,
      (System.nanoTime - wall0) / 1e9)
  }

  /** The seeds `dir`'s slice replays: [[Seeds]], and the seeds beyond them it was found red on, one a line in its `seeds`
   *  file (a rotating seed of the convergence legs runs to 64): a red seed stays held once its slice is committed. */
  def seedsOf(dir: Path): Seq[Int] = {
    val file  = dir.resolve("seeds")
    val extra = if (!Files.exists(file)) Nil
                else Files.readAllLines(file).asScala.toSeq.map(_.trim).filter(l => l.nonEmpty && !l.startsWith("#")).map(_.toInt)
    (Seeds ++ extra).distinct
  }

  /** The replay's progress lines, kept off the suite's output. */
  private def quietly[A](body: => A): A = {
    val sink = new java.io.PrintStream(java.io.OutputStream.nullOutputStream())
    Console.withOut(sink)(Console.withErr(sink)(body))
  }
}

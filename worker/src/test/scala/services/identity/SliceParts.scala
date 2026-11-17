package services.identity

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._

/**
 * THE REAL-FAMILY SPECS IN PARTS: [[TitleRelationSpec]], [[ReadSetClosureSpec]] and [[PopulationMonotonicitySpec]] each
 * check every committed slice, minutes of resolves apiece (400-690 s in the landing check), so one class was the floor no
 * identity shard went below (scripts/ci/shard_tests.py splits by class). Each is [[Count]] classes (`<Spec>Part<k>Spec`),
 * the slices cut into parts by MEASURED COST ([[CostsFile]]), heaviest first onto the lightest part: every slice is in
 * exactly one part, and part 1 also holds the spec's checks of no single slice. (Until 2026-11-17 part k was the slices
 * whose name hashed to k: Part2 drew the two heaviest identity slices, ~263 of ~700 cpu-s, and at 635 s on Main was
 * `test (worker-identity-2)`'s floor while Part3 had ~95.) [[All]] is every slice and every check in
 * one suite (what the parts together are: RealFamilyPartsSpec proves it, case for case).
 */
object SliceParts {

  /** Into how many parts each real-family spec splits the slices. */
  val Count = 4

  /** Not a part: every slice and every check. */
  val All = 0

  /** Each slice's measured cost, `<slice under the fixtures>` tab seconds a line (`#` comments). */
  val CostsFile: Path = Path.of("test", "resources", "fixtures", "real-family-slice-costs.tsv")

  /** What a slice [[CostsFile]] does not list weighs: most walk and close in 1-3 s. */
  val UnmeasuredSeconds = 2.5

  lazy val costs: Map[String, Double] =
    if (!Files.exists(CostsFile)) Map.empty
    else Files.readAllLines(CostsFile).asScala.toSeq.map(_.trim).filter(l => l.nonEmpty && !l.startsWith("#")).map { line =>
      val Array(name, seconds) = line.split("\t", 2)
      name -> seconds.trim.toDouble
    }.toMap

  def costOf(dir: Path): Double = costs.getOrElse(PopulationMonotonicitySpec.relative(dir), UnmeasuredSeconds)

  /** `dirs` cut into [[Count]] parts, longest-processing-time first: the heaviest slice first (ties by name), each onto
   *  the part carrying least so far (ties the lowest). Deterministic: every JVM cuts the same parts. */
  def parts(dirs: Seq[Path]): Map[Path, Int] = {
    val load = Array.fill(Count)(0.0)
    dirs.distinct.sortBy(d => (-costOf(d), PopulationMonotonicitySpec.relative(d))).map { d =>
      val i = load.indices.minBy(i => (load(i), i))
      load(i) += costOf(d)
      d -> (i + 1)
    }.toMap
  }

  private lazy val committed = parts(PopulationMonotonicitySpec.sliceDirs)

  /** The part (1 to [[Count]]) checking `dir`'s slice, of the committed slices' cut. */
  def partOf(dir: Path): Int = committed.getOrElse(dir, parts(PopulationMonotonicitySpec.sliceDirs :+ dir)(dir))

  /** The slices of `dirs` that `part` checks. */
  def of(part: Int, dirs: Seq[Path]): Seq[Path] = {
    require(part == All || (1 to Count).contains(part), s"part $part: not 1 to $Count, nor All")
    if (part == All) dirs else { val cut = parts(dirs); dirs.filter(cut(_) == part) }
  }

  /** Whether `part` holds the spec's checks of no single slice. */
  def holdsWholeChecks(part: Int): Boolean = part == All || part == 1

  /** The slices of `dirs` in the countries a [[tools.CountryScope]] test may check: the rest cancel before they read
   *  a result, so they are never resolved. */
  def inScope(dirs: Seq[Path], countries: Set[String]): Seq[Path] =
    dirs.filter(dir => countries(PopulationMonotonicitySpec.countryOf(dir)))
}
